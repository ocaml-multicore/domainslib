open Domainslib
module T = Domainslib.Task

module type V = sig
  type t
end

module BatchedBtree(V : V) = struct

  type t = V.t Btree.t 
  type _ batch_op = 
    | Search : int -> V.t option batch_op
    | Insert : int * V.t -> unit batch_op

  type wrapped_batch_op = 
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  type setter =
    | Search_set of (V.t option -> unit)
  let create () = Btree.create ()

  let batch_limit = 20
  let insert_chan = Chan.make_bounded batch_limit
  (* let search_chan = Chan.make_bounded batch_limit *)
  let search_chan: (V.t option T.promise * V.t option -> unit) Chan.t = Chan.make_bounded batch_limit
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
        Chan.send insert_chan (key, value); set ()
    done;
    (* Inserts *)
    let n = populate insert_batch insert_chan in
    for i = 0 to n-1 do
      let key, value = Option.get insert_batch.(i) in
      Btree.insert t key value
    done;
    (* Wait for Searches *)
    while 
      let pr_op = Chan.recv_poll search_chan in
      match pr_op with
      | Some (pr, Search_set set) -> set @@ Task.await pool pr; true
      | None -> false 
    do ()
    done
end
(* 
module PolyBatchedBtree = struct

  type 'elt t = 'elt Btree.t 
  type ('elt, _) batch_op = 
    | Search : int -> ('elt, 'elt option) batch_op
    | Insert : int * 'elt -> ('elt, unit) batch_op

  type wrapped_batch_op = 
      Batched_op : ('elt, 'a) batch_op * ('a -> unit) -> wrapped_batch_op

  let create () = Btree.create ()

  let search_list = ref []

  let bop : 'a t -> T.pool -> wrapped_batch_op array -> int -> unit = 
    fun t pool bop_arr n ->
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, _) -> 
        search_list := key :: !search_list;
      | Batched_op (Insert (key, value), set) -> 
        Btree.insert t key value; set ();
    done;
    let op_arr = Btree.par_search ~pool t (Array.of_list !search_list) in
    let i = ref 0 in
    Array.iter (fun (res : 'a option) -> 
        while match bop_arr.(!i) with 
            Batched_op (Search _, _) -> false 
          | _ -> true do
          incr i
        done;
        match bop_arr.(!i) with 
        | Batched_op (Search _, set) -> set res 
        | _ -> failwith "Impossible") op_arr
end *)

include Batcher.Make(BatchedBtree(String))
let search t i = batchify t (Search i)
let insert t k v = batchify t (Insert (k, v))


