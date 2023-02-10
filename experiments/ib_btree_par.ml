open Domainslib
module T = Domainslib.Task
module Btree = Batch_para_btree

module type V = sig
  type t
end

module ExBatchedBtree(V : V) = struct

  type t = V.t Btree.t 
  type _ batch_op = 
    | Search : int -> V.t option batch_op
    | Insert : int * V.t -> unit batch_op

  type wrapped_batch_op = 
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let create () = Btree.create ()

  let batch_limit = 20
  let insert_chan = Chan.make_bounded batch_limit
  (* let search_chan = Chan.make_bounded batch_limit *)
  let search_chan: ((V.t option T.promise) * (V.t option -> unit)) Chan.t = Chan.make_bounded batch_limit
  let insert_batch : (int * V.t) option array = Array.make batch_limit None
  let populate arr chan =
    let i = ref 0 in
    while 
      match Chan.recv_poll chan with
      | Some _ as v -> arr.(!i) <- v; incr i; true
      | None -> false
    do ()
    done; !i

  let bop : V.t Btree.t -> T.pool -> wrapped_batch_op array -> int -> unit = 
    fun t pool bop_arr n ->
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, set) -> 
        let pr = Task.async pool (fun () -> Btree.search t key) in
        Chan.send search_chan (pr, set);
      | Batched_op (Insert (key, value), set) -> 
        set @@ Btree.insert t key value
        (* Chan.send insert_chan (key, value); set () *)
    done;
    (* Wait for Searches *)
    while 
      let pr_op = Chan.recv_poll search_chan in
      match pr_op with
      | Some (pr, set) -> set @@ Task.await pool pr; true
      | None -> false 
    do ()
    done;
    (* Inserts *)
    let n = populate insert_batch insert_chan in
    for i = 0 to n-1 do
      let[@warning "-26"] key, value = Option.get insert_batch.(i) in
      (* Btree.insert t key value *)()
    done;
end

(* Define Implicit Batching version *)
module ImpBatchedBtree(V : V) = struct
  open Batcher.Make(ExBatchedBtree(V))
  let create = create
  let search t i = batchify t (Search i)
  let insert t k v = batchify t (Insert (k, v))
end
