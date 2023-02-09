open Domainslib

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

  let[@warning "-27"] bop : V.t Btree.t -> Task.pool -> wrapped_batch_op array -> int -> unit = 
    fun t _pool bop_arr n ->
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, set) -> 
        set @@ None
      | Batched_op (Insert (key, value), set) -> 
        set @@ Btree.insert t key value 
    done;
end

(* Define Implicit Batching version *)
module ImpBatchedBtree(V : V) = struct
  open Batcher.Make(ExBatchedBtree(V))
  let create = create
  let search t i = batchify t (Search i)
  let insert t k v = batchify t (Insert (k, v))
end