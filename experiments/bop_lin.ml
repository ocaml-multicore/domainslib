(* Linearizability Tester doesn't quite work for our use case because we have the wrap it over the implicit batcher which creates some issue with the execution  *)

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


(* The alternative solution is to take some set of executions and see if the result can be reproduced with some sequential ordering exists for it *)

module ExBatchedBtree = Ib_btree_one.ExBatchedBtree(String)
module SeqBtree = Btree

type 'a op = Ins of int * 'a | Search of int 
let pool = Domainslib.Task.setup_pool ~num_domains:3 ()
let ops : ('a op) list= []

let conv_seq_op : 'a op -> ('a SeqBtree.t -> unit) = failwith "unimplemeneted"

let conv_batch_op : 'a op -> ExBatchedBtree.wrapped_batch_op = failwith "unimplemented"

let rec interleave e seen = function
    [] -> [seen @ [e]]
  | x :: xs -> (seen @ e :: x :: xs) :: interleave e (seen @ [x]) xs
let combine x ps =
  List.concat (List.map (interleave x []) ps)
let rec perms = function
    [] -> [[]]
  | h :: t -> combine h (perms t)

let seq_exec batch : ExBatchedBtree.t Iter.t =
  let seq_op = List.map conv_seq_op batch in
  let seq_perms = perms seq_op in
  Iter.of_list seq_perms |>
  Iter.map (fun exec_order -> 
      let t = Btree.create () in
      List.iter (fun op -> op t) exec_order;
      t
    )

let bop_exec batch = 
  let batch_ops = List.map conv_batch_op batch |> Array.of_list in
  let t = ExBatchedBtree.create () in
  ExBatchedBtree.bop t pool batch_ops (Array.length batch_ops);
  t

let check_linearizable () =
  let bop_res = bop_exec ops in
  let seq_res = seq_exec ops in
  Iter.exists (fun exec -> exec = bop_res) seq_res