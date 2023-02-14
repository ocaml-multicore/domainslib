open Domainslib

(* Define Explicitly Batched version of data structure *)
module ExBatchedBtree = struct
  type 'a t = 'a Btree.t 
  type ('a, _) op = 
    | Search : int -> ('a, 'a option) op
    | Insert : int * 'a -> ('a, unit) op

  type 'a wrapped_op = 
      Mk : ('a, 'b) op * ('b -> unit) -> 'a wrapped_op

  let init () = Btree.create ()

  let[@warning "-27"] run
    : 'a Btree.t -> Task.pool -> 'a wrapped_op array -> unit = 
    fun (type a) (t: a Btree.t) _pool (bop_arr: a wrapped_op array) ->
    for i = 0 to Array.length bop_arr -1 do
      match (bop_arr.(i): a wrapped_op) with
      | Mk (Search key, set) -> 
        set @@ None
      | Mk (Insert (key, value), set) -> 
        set @@ Btree.insert t key value 
    done
end

(* Define Implicit Batching version *)
module ImpBatchedBtree = struct
  open Batcher.Make1(ExBatchedBtree)
  let create = init
  let search t i = apply t (Search i)
  let insert t k v = apply t (Insert (k, v))
end
