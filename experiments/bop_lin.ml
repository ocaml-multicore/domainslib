(* Linearizability Tester doesn't quite work for our use case because we have the wrap it over the implicit batcher which creates some issue with the execution  *)

(* 

  The alternative solution is to take some set of executions and see if the result can be reproduced with some sequential ordering exists for it.

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
  | Null

let pool = Domainslib.Task.setup_pool ~num_domains:3 ()
let batch : op list = 
  [Search (2); Ins (1, "key 1"); Search (1); Ins (2, "key 2")]

let pp_op ppf (op : op) = match op with
  | Ins (k,v) -> Format.fprintf ppf "Insert(%d, %s)" k v
  | Search k -> Format.fprintf ppf "       Search(%d)" k

let pp_result ppf = function
  | Ins _ -> Format.fprintf ppf "()"
  | Search value -> 
    (match value with
     | Some s -> Format.fprintf ppf "Some(%s)" s
     | None -> Format.fprintf ppf "None")
  | Null -> failwith "Bad Case"

let[@warning "-32"] pp_result_list ppf a =
  Format.fprintf ppf "@[<v>@ Start@ ";
  List.iter (fun elt -> Format.fprintf ppf "-> %a@ " pp_result elt) a;
  Format.fprintf ppf "End@]"

let[@warning "-32"] pp_op_list ppf l =
  Format.fprintf ppf "[|@[<v>@ ";
  List.iter (fun elt -> Format.fprintf ppf "%a@ " pp_op elt) l;
  Format.fprintf ppf "@]%20s|]@." " "

let pp_combine_list ppf op_res =
  let op_l, res_l = op_res in 
  List.iter2 (fun res op -> 
      Format.fprintf ppf "-> %a ==> %a@ " pp_op op pp_result res) res_l op_l

let conv_seq_op : op -> ('a SeqBtree.t -> result) = function 
  | Ins (k, v) -> fun t -> Ins (Btree.insert t k v)
  | Search k -> fun t -> Search (Btree.search t k)

let conv_batch_op (op : op) res_arr i =
  let open Domainslib in
  match op with
  | Ins (k,v) -> 
    let pr, set = Task.promise () in
    let wrapped_set res = 
      set res;
      res_arr.(i) <- (Ins (Task.await pool pr)) in
    ExBatchedBtree.Batched_op (ExBatchedBtree.Insert (k,v), wrapped_set)
  | Search k -> 
    let pr, set = Task.promise () in
    let wrapped_set res = 
      set res;
      res_arr.(i) <- (Search (Task.await pool pr)) in
    ExBatchedBtree.Batched_op (ExBatchedBtree.Search k, wrapped_set)

let snd (_,s,_) = s 
let thrd (_,_,thrd) = thrd

let rec interleave e seen = function
    [] -> [seen @ [e]]
  | x :: xs -> (seen @ e :: x :: xs) :: interleave e (seen @ [x]) xs
let combine x ps =
  List.concat (List.map (interleave x []) ps)
let rec perms = function
    [] -> [[]]
  | h :: t -> combine h (perms t)

let[@warning "-32"] seq_exec batch = 
  let batch_i = List.mapi (fun i op -> i, op, conv_seq_op op) batch in
  let seq_perms = perms batch_i in
  let itr = Iter.of_list seq_perms in
  Iter.map (fun op_list -> 
      let t = Btree.create () in
      List.map (fun (i,op, conv_op) -> i, op, conv_op t) op_list 
    ) itr

(* let[@warning "-32"] print_all_seq batch = 
   let snd (_,s,_) = s in
   let res = seq_exec batch in
   let ordered_seq = Iter.map (fun seq -> 
      let sorted = List.sort (fun (i1,_,_) (i2,_,_) -> Int.compare i1 i2) seq in
      List.map snd sorted |> Array.of_list) res in
   Iter.iter (fun seq -> Format.printf "%a" pp_result_list seq) ordered_seq *)

let[@warning "-32"] bop_exec batch = 
  let batch_sz = List.length batch in
  let res_array = Array.make batch_sz Null in
  let batch_ops = List.mapi (fun i op -> conv_batch_op op res_array i) batch 
                  |> Array.of_list in
  let t = ExBatchedBtree.create () in
  ExBatchedBtree.bop t pool batch_ops batch_sz;
  res_array
(* let[@warning "-32"] print_bop () = 
   Domainslib.Task.run pool (fun () -> 
      Format.printf "%a" pp_result_list @@ bop_exec batch
    );
   Domainslib.Task.teardown_pool pool *)

let check_linearizable () =
  let res = Domainslib.Task.run pool (fun () -> 
      let bop_res = bop_exec batch |> Array.to_list in
      let seq_res = seq_exec batch in
      let itr = Iter.filter (fun exec -> 
          let bop_order = List.sort (fun (i1,_,_) (i2,_,_) 
                                      -> Int.compare i1 i2) exec in
          let cleaned_bop_order = List.map thrd bop_order in
          cleaned_bop_order <> bop_res) seq_res in
      Iter.length itr, itr
    ) in
  Domainslib.Task.teardown_pool pool;
  res

let () = 
  print_newline ();
  let linearizable, seq_res = check_linearizable () in
  if linearizable > 0 then begin
    Format.printf "@[<v>@ A Sequential ordering was found for the batch!@ @ Valid Orderings:@]@.";
    Iter.iter (fun res -> let seq_op_ordering = List.map snd res in
                let seq_res_ordering = List.map thrd res in
                Format.printf "@[<v>%a@ @]@." pp_combine_list 
                  (seq_op_ordering, seq_res_ordering)) seq_res
  end
  else 
    Format.printf "@[<v>@ No Sequential ordering exists for the batch!@ %a@]@." pp_op_list batch;
