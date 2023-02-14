(* 

1. Take any sequential data structure, and apply it to the Batcher.Make(DS) functor, following this generic template. 

2. You now have a concurrent version of your Data Structure that is always sequentially consistent. The reason why it is thread-safe is because the synchronization is handled by the implicit batching scheduler. To wit, we can submit operations to the DS concurrently but they do not execute concurrently YET.

3. Parallelism can then be incrementally added on to the DS in a modular way per operation because we have control over when to run the concurrent operations and the sequential operations

EXTENSIONS:
1. [EX_BATCH Interface] Perhaps we can add some generics at the level of bop to allow you to automatically and efficiently optimize based on the properties of the operations, E.g. commutative, associative... 

2. [Batcher Interface] We can provide plugins that provide more metadata about the operations or partition them efficiently and automatically returning a 
Map(Op -> Op array) instead of just an array

*)
open Domainslib
module T = Domainslib.Task

module type V = sig
  type t
end

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
        set @@ Btree.search t key
      | Mk (Insert (key, value), set) -> 
        set @@ Btree.insert t key value 
    done
end

(* Define Implicit Batching version *)
module ImpBatchedBtree(V : V) = struct
  open Batcher.Make1(ExBatchedBtree)
  let create = init
  let search t i = apply t (Search i)
  let insert t k v = apply t (Insert (k, v))
end

(* Main program *)
(* let inserts = 1_000_000
   let main pool () =
   let module S_Btree = ImpBatchedBtree(String) in
   let t = S_Btree.create pool in
   T.parallel_for pool ~start:1 ~finish:inserts ~body:(fun i ->
      let value = "key" ^ string_of_int i in
      S_Btree.insert t i value
    );
   assert (S_Btree.search t (inserts/2) |> Option.is_some) 

   let () = 
   let pool = Task.setup_pool ~num_domains:7 () in
   Utils.time (fun () -> Task.run pool (main pool));
   Task.teardown_pool pool *)
