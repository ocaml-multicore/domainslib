module Seq = Ib_btree_seq
module Seq_flipped = Ib_btree_seq_flipped
module Seq_bad = Ib_btree_seq_bad
module Par = Ib_btree_par

open Domainslib
module T = Domainslib.Task
module Btree = Batch_para_btree

(* Define Explicitly Batched version of data structure *)
module ExBatchedBtree = struct

  type 'a t = 'a Btree.t 
  type ('a, _) op = 
    | Search : int -> ('a, 'a option) op
    | Insert : int * 'a -> ('a, unit) op

  type 'a wrapped_op = 
      Mk : ('a, 'b) op * ('b -> unit) -> 'a wrapped_op

  let init () = Btree.create ()

  let sz = 1_000_000

  let run : 'a Btree.t -> Task.pool -> 'a wrapped_op array -> unit = 
    fun (type a) (t: a Btree.t) pool (bop_arr :a wrapped_op array) ->
    let batch_insert = ref [] in
    for i = 0 to Array.length bop_arr - 1 do
      match (bop_arr.(i): a wrapped_op) with
      | Mk (Search key, set) -> 
        set @@ Btree.search t key
      | Mk (Insert (key, value), set) -> 
        set @@ Btree.insert t key value;
        batch_insert := (key, value) :: !batch_insert;
    done;
    let batch_insert = Array.of_list !batch_insert in
    Batch_para_btree.par_insert_rebuilder ~pool t batch_insert
end

(* Define Implicit Batching version *)
module ImpBatchedBtree = struct
  open Batcher.Make1(ExBatchedBtree)
  let create = init
  let search t i = apply t (Search i)
  let insert t k v = apply t (Insert (k, v))
end


include Batcher.Make1(ExBatchedBtree)
let search t i = apply t (Search i)
let insert t k v = apply t (Insert (k, v))

