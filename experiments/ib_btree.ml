open Domainslib
module Btree = Batch_para_btree

module type V = sig
  type t
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

  let bop : V.t Btree.t -> Task.pool -> wrapped_batch_op array -> int -> unit = 
    fun t _pool bop_arr n ->
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, set) -> 
        set @@ Btree.search t key
      | Batched_op (Insert (key, value), set) -> 
        set @@ Btree.insert t key value
    done
end

include Batcher.Make(ExBatchedBtree(String))
let search t i = batchify t (Search i)
let insert t k v = batchify t (Insert (k, v))