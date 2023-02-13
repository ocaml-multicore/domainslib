open Domainslib
module T = Domainslib.Task
module Btree = Batch_para_btree

module ExBatchedBtree = struct

  type 'a t = 'a Btree.t 
  type ('a, _) op = 
    | Search : int -> ('a, 'a option) op
    | Insert : int * 'a -> ('a, unit) op

  type 'a wrapped_op = 
      Mk : ('a, 'b) op * ('b -> unit) -> 'a wrapped_op

  let init () = Btree.create ()

  let batch_limit = 20
  let populate arr chan =
    let i = ref 0 in
    while 
      match Chan.recv_poll chan with
      | Some _ as v -> arr.(!i) <- v; incr i; true
      | None -> false
    do ()
    done; !i

  let run : 'a Btree.t -> T.pool -> 'a wrapped_op array -> unit = 
    fun (type a) (t: a Btree.t) pool (bop_arr: a wrapped_op array) ->
    let insert_chan = Chan.make_bounded batch_limit in
    (* let search_chan = Chan.make_bounded batch_limit *)
    let search_chan: ((a option T.promise) * (a option -> unit)) Chan.t =
      Chan.make_bounded batch_limit in
    let insert_batch : (int * 'a) option array = Array.make batch_limit None in
    for i = 0 to Array.length bop_arr -1 do
      match (bop_arr.(i): a wrapped_op) with
      | Mk (Search key, set) -> 
        let pr = Task.async pool (fun () -> Btree.search t key) in
        Chan.send search_chan (pr, set);
      | Mk (Insert (key, value), set) -> 
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
module ImpBatchedBtree = struct
  open Batcher.Make1(ExBatchedBtree)
  let create = init
  let search t i = apply t (Search i)
  let insert t k v = apply t (Insert (k, v))
end
