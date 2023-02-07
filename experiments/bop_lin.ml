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


(* 

  The alternative solution is to take some set of executions and see if the result can be reproduced with some sequential ordering exists for it. This is on a best effort situation whereby the order of the results are concurrently published to a MP Channel. There is a data race here whereby the published result can be invalid

   A way to counter this is to check the state of the data structure at the end of the batch insert and test that against a sequential version. However, this relies on the semantics of the batch operations that must ensure that it will not perform optimizing operations that change the shape of the DS but preserve it's properties

*)

module ExBatchedBtree = Ib_btree_one.ExBatchedBtree(String)
module SeqBtree = Btree

type [@warning "-37"] op = 
  | Ins of int * string 
  | Search of int 

type result =
  | Ins of unit
  | Search of string option

let pp_op ppf (op : op) = match op with
  | Ins (k,v) -> Format.fprintf ppf "Insert(%d, %s)" k v
  | Search k -> Format.fprintf ppf "       Search(%d)" k

let pp_result ppf = function
  | Ins _ -> Format.fprintf ppf "()"
  | Search value -> 
    match value with
    | Some s -> Format.fprintf ppf "Some(%s)" s
    | None -> Format.fprintf ppf "None"

let pp_result_list ppf a =
  Format.fprintf ppf "@[<v>@ Start@ ";
  Array.iter (fun elt -> Format.fprintf ppf "-> %a@ " pp_result elt) a;
  Format.fprintf ppf "End@]"

let[@warning "-32"] pp_op_list ppf a =
  Format.fprintf ppf "[|@[<v>@ ";
  List.iter (fun elt -> Format.fprintf ppf "%a@ " pp_op elt) a;
  Format.fprintf ppf "@]%20s|]@." " "

let pp_combine_list ppf res_op =
  let res_a, op_l = res_op in 
  let res_l = Array.to_list res_a in
  List.iter2 (fun res op -> 
      Format.fprintf ppf "-> %a ==> %a@ " pp_op op pp_result res) res_l op_l

let pool = Domainslib.Task.setup_pool ~num_domains:3 ()
let ops : op list = [Search (2); Ins (1, "key 1"); Search (1); Ins (2, "key 2")]

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
  let seq_perms = perms batch in
  let seq_op = List.map (fun l -> List.map conv_seq_op l, l) seq_perms in
  let itr = Iter.of_list seq_op in
  Iter.map (fun (exec_order,op_list) -> 
      let t = Btree.create () in
      let res = List.map (fun op -> op t) exec_order 
                |> Array.of_list in
      res, exec_order, op_list
    ) itr

let[@warning "-32"] print_seq () = 
  let res = seq_exec ops |> Iter.map (fun (fst,_,_) -> fst) in
  Iter.iter (fun seq -> Format.printf "%a" pp_result_list seq) res

let[@warning "-32"] bop_exec batch : result array = 
  let batchsz = List.length batch in
  let chan = Domainslib.Chan.make_bounded batchsz in
  let batch_ops = List.map (fun op -> conv_batch_op op chan) batch |> Array.of_list in
  let t = ExBatchedBtree.create () in
  ExBatchedBtree.bop t pool batch_ops (Array.length batch_ops);
  Array.init batchsz (fun _ -> Domainslib.Chan.recv chan)

let[@warning "-32"] print_bop () = 
  Domainslib.Task.run pool (fun () -> 
      Format.printf "%a" pp_result_list @@ bop_exec ops
    );
  Domainslib.Task.teardown_pool pool

let check_linearizable () =
  let res = Domainslib.Task.run pool (fun () -> 
      let bop_res = bop_exec ops in
      let seq_res = seq_exec ops in
      let itr = Iter.filter (fun (exec,_,_) -> exec <> bop_res) seq_res in
      not @@ Iter.is_empty itr, itr, bop_res
    ) in
  Domainslib.Task.teardown_pool pool;
  res

let () = 
  print_newline ();
  let linearizable, seq_res, _bop_res = check_linearizable () in
  if linearizable then
    Format.printf "@[<v>@ A Sequential ordering was found for the batch!@ %a@]@." pp_op_list ops
  else 
    Format.printf "@[<v>@ No Sequential ordering exists for the batch!@ %a@]@." pp_op_list ops;
  let seq_res_ordering,_,seq_op_ordering = Iter.head_exn seq_res in
  Format.printf "@[<v>Ordering:@ %a@]@." pp_combine_list (seq_res_ordering, seq_op_ordering)