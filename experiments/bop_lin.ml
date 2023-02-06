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

type [@warning "-37"] op = 
  | Ins of int * string 
  | Search of int 

type result =
  | Ins of unit
  | Search of string option
[@@deriving show]

let print_result a =
  let l = Array.to_list a in
  let pp_l ppf = Format.pp_print_list pp_result ppf in
  Format.printf "@[ [|%a|]@; @]@." pp_l l

let pool = Domainslib.Task.setup_pool ~num_domains:3 ()
let ops : op list= [Search (2); Ins (1, "key 1"); Search (1); Ins (2, "key2")]

let conv_seq_op : op -> ('a SeqBtree.t -> result) = function 
  | Ins (k, v) -> fun t -> Ins (Btree.insert t k v)
  | Search k -> fun t -> Search (Btree.search t k)

let conv_batch_op (op : op) (chan : result Domainslib.Chan.t) =
  let open Domainslib in
  match op with
  | Ins (k,v) -> 
    let pr, set = Task.promise () in
    let wrapped_set res = 
      set res;
      Domainslib.Chan.send chan (Ins (Task.await pool pr)) in
    ExBatchedBtree.Batched_op (ExBatchedBtree.Insert (k,v), wrapped_set)
  | Search k -> 
    let pr, set = Task.promise () in
    let wrapped_set res = 
      set res;
      Domainslib.Chan.send chan (Search (Task.await pool pr)) in
    ExBatchedBtree.Batched_op (ExBatchedBtree.Search k, wrapped_set)

let rec interleave e seen = function
    [] -> [seen @ [e]]
  | x :: xs -> (seen @ e :: x :: xs) :: interleave e (seen @ [x]) xs
let combine x ps =
  List.concat (List.map (interleave x []) ps)
let rec perms = function
    [] -> [[]]
  | h :: t -> combine h (perms t)

let[@warning "-32"] seq_exec batch =
  let seq_op = List.map conv_seq_op batch in
  let seq_perms = perms seq_op in
  let itr = Iter.of_list seq_perms in
  Iter.map (fun exec_order -> 
      let t = Btree.create () in
      List.map (fun op -> op t) exec_order 
      |> Array.of_list
    ) itr
let[@warning "-32"] print_seq () = Iter.iter print_result (seq_exec ops)
let[@warning "-32"] bop_exec batch : result array = 
  let batchsz = List.length batch in
  let chan = Domainslib.Chan.make_bounded batchsz in
  let batch_ops = List.map (fun op -> conv_batch_op op chan) batch |> Array.of_list in
  let t = ExBatchedBtree.create () in
  ExBatchedBtree.bop t pool batch_ops (Array.length batch_ops);
  Array.init batchsz (fun _ -> Domainslib.Chan.recv chan)

let[@warning "-32"] print_bop () = 
  Domainslib.Task.run pool (fun () -> 
      print_result @@ bop_exec ops 
    );
  Domainslib.Task.teardown_pool pool

let check_linearizable () =
  let res = Domainslib.Task.run pool (fun () -> 
      let bop_res = bop_exec ops in
      print_result bop_res;
      let seq_res = seq_exec ops in
      Iter.exists (fun exec -> exec = bop_res) seq_res
    ) in
  Domainslib.Task.teardown_pool pool;
  res

let () = 
  print_newline ();
  if check_linearizable () then 
    Format.printf "@[Sequential ordering found!@]@."
  else 
    Format.printf "@[No Sequential ordering exists!@]@."