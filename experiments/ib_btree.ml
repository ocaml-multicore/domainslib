module Seq = Ib_btree_seq
module Seq_flipped = Ib_btree_seq_flipped
module Seq_bad = Ib_btree_seq_bad
module Par = Ib_btree_par

open Domainslib
module T = Domainslib.Task
module Btree = Batch_para_btree

module type V = sig
  type t
  val empty : t
end

(* Define Explicitly Batched version of data structure *)
module ExBatchedBtree(V : V) = struct

  type t = V.t Btree.t
  type _ batch_op =
    | Search : int -> V.t option batch_op
    | Insert : int * V.t -> unit batch_op

  type wrapped_batch_op =
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let create () = Btree.create ()
  let sz = 1_000_000
  let batch_insert = Array.make sz (0, V.empty)

  let bop : V.t Btree.t -> Task.pool -> wrapped_batch_op array -> int -> unit = 
    fun t pool bop_arr n ->
    assert (n < sz);
    let idx = ref 0 in
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, set) -> 
        set @@ Btree.search t key
      | Batched_op (Insert (key, value), set) -> 
        set @@ Btree.insert t key value;
        batch_insert.(!idx) <- (key, value);
        incr idx;
    done;
    Batch_para_btree.par_insert_rebuilder ~pool t batch_insert
end

(* Define Implicit Batching version *)
module ImpBatchedBtree(V : V) = struct
  open Batcher.Make(ExBatchedBtree(V))
  let create = create
  let search t i = batchify t (Search i)
  let insert t k v = batchify t (Insert (k, v))
end


include Batcher.Make(ExBatchedBtree(String))
let search t i = batchify t (Search i)
let insert t k v = batchify t (Insert (k, v))

