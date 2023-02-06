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
        set @@ Btree.search t key
      | Batched_op (Insert (key, value), set) -> 
        set @@ Btree.insert t key value 
    done
end

(* Define Implicit Batching version *)
module ImpBatchedBtree(V : V) = struct
  open Batcher.Make(ExBatchedBtree(V))
  let create = create
  let search t i = batchify t (Search i)
  let insert t k v = batchify t (Insert (k, v))
end

(* Main program *)
let inserts = 10_000_000
let main pool () =
  let module S_Btree = ImpBatchedBtree(String) in
  let t = S_Btree.create pool in
  for i = 1 to inserts do
    let value = "Key" ^ string_of_int i in
    S_Btree.insert t i value
  done;
  assert (S_Btree.search t (inserts/2) |> Option.is_some)

(* let () = 
   let pool = Task.setup_pool ~num_domains:7 () in
   Utils.time (fun () -> Task.run pool (main pool));
   Task.teardown_pool pool *)

(* Linearizability Tester doesn't quite work for our use case because we have the wrap it over the implicit batcher which creates some issue with the execution  *)

(* The alternative solution is to take some set of executions and see if the result can be reproduced with some sequential ordering exists for it *)

(* 
module ExDS =
struct
  module String_BTree = ImpBatchedBtree(String)
  type t = String_BTree.t
  let pool = ref None
  let init _ = 
    let new_pool = Task.setup_pool ~num_domains:1 () in
    pool := Some new_pool;
    String_BTree.create new_pool
  let cleanup _ = 
    match !pool with
    | Some p ->  Task.teardown_pool p
    | None -> ()

  open Lin
  let a,b = nat_small,string_small_printable

  let insert t k v =
    let pool = Option.get !pool in
    Task.run pool (fun () -> String_BTree.insert t k v)
  let search t k =
    let pool = Option.get !pool in
    Task.run pool (fun () -> String_BTree.search t k)
  let api =
    [ val_ "Btree.insert" insert (t @-> a @-> b @-> returning unit);
      val_ "Btree.search" search (t @-> a @-> returning (option b)); ]
end

module SBT = Lin_domain.Make(ExDS)
let () = 
  QCheck_base_runner.run_tests_main [
    SBT.lin_test ~count:1 ~name:"ExBatchedBtree DSL test";
  ] *)
