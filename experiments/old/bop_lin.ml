(* Linearizability Tester doesn't quite work for our use case because we have the wrap it over the implicit batcher which creates some issue with the execution  *)

(* 

  The alternative solution is to take the result of the batch and compare it against all the permutations of sequential orderings and check if there exists an ordering that matches the result of the BOP.

  Potential Improvements:
  1. We can run some number of random operations in prior and then save the state of the DS and then perform the BOP linearizability to increase the likelihood of bugs
  2. Make this more automated and generic to generate

  Our Delta:
  We can implement some annotations to indicate properties of the operations.
  E.g. if a operation is commutative, we can reduce the search space of the permuations by dropping permuations that are commutatively equivalent

*)

module ExBatchedBtree = Ib_btree.Seq.ExBatchedBtree
module SeqBtree = Btree

type [@warning "-37"] op = 
  | Ins of int * string 
  | Search of int 

type result =
  | Ins of unit
  | Search of string option
  | Null

let pp_op ppf (op : op) = match op with
  | Ins (k,v) -> 
    let ins_s = Format.sprintf "Insert(%d, %s)" k v in
    Format.fprintf ppf "%16s" ins_s
  | Search k -> 
    let search_s = Format.sprintf "Search(%d)" k in
    Format.fprintf ppf "%16s" search_s

let pp_result ppf = function
  | Ins _ -> Format.fprintf ppf "%16s" "()"
  | Search value -> 
    (match value with
     | Some s -> 
       let s = Format.sprintf "Some(%s)" s in
       Format.fprintf ppf "%16s" s
     | None -> Format.fprintf ppf "%16s" "None")
  | Null -> failwith "Bad Case"

let pp_batch_op_res ppf (ops, res) =
  Format.fprintf ppf "@[<hov> Ops : [|";
  List.iter (fun elt -> Format.fprintf ppf "; %a@ " pp_op elt) ops;
  Format.fprintf ppf "|]@]@.";
  Format.fprintf ppf "@[<hov> Res : [|";
  List.iter (fun elt -> Format.fprintf ppf "; %a@ " pp_result elt) res;
  Format.fprintf ppf "|]@]"

let pp_combine_list ppf op_res =
  let op_l, res_l = op_res in 
  List.iter2 (fun res op -> 
      Format.fprintf ppf "-> %a ==> %a@ " pp_op op pp_result res) res_l op_l

let conv_seq_op : op -> ('a SeqBtree.t -> result) = function 
  | Ins (k, v) -> fun t -> Ins (Btree.insert t k v)
  | Search k -> fun t -> Search (Btree.search t k)

let conv_batch_op (op : op) pool res_arr i =
  let open Domainslib in
  match op with
  | Ins (k,v) -> 
    let pr, set = Task.promise () in
    let wrapped_set res = 
      set res;
      res_arr.(i) <- (Ins (Task.await pool pr)) in
    ExBatchedBtree.Mk (ExBatchedBtree.Insert (k,v), wrapped_set)
  | Search k -> 
    let pr, set = Task.promise () in
    let wrapped_set res = 
      set res;
      res_arr.(i) <- (Search (Task.await pool pr)) in
    ExBatchedBtree.Mk (ExBatchedBtree.Search k, wrapped_set)

let snd (_,s,_) = s 
let thrd (_,_,thrd) = thrd

let preset : op list = 
  [Ins (1, "key 1")]
let batch : op list = 
  [Search (2); Ins (2, "key 2"); Search (1)]

let init_seq_t () =
  let t = SeqBtree.create () in
  List.iter (fun op -> let _ = conv_seq_op op t in ()) preset;
  t

let init_batch_t pool =
  let t = ExBatchedBtree.init () in
  let preset_n = List.length preset in
  let preset_result_a = Array.make preset_n Null in
  let batch = List.mapi (fun i op -> conv_batch_op op pool preset_result_a i) preset |> Array.of_list in
  ExBatchedBtree.run t pool batch;
  t

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
      let t = init_seq_t () in
      List.map (fun (i,op, conv_op) -> i, op, conv_op t) op_list 
    ) itr

let[@warning "-32"] bop_exec pool batch = 
  let batch_sz = List.length batch in
  let res_array = Array.make batch_sz Null in
  let batch_ops = List.mapi (fun i op -> conv_batch_op op pool res_array i) batch |> Array.of_list in
  let t = init_batch_t pool in
  ExBatchedBtree.run t pool batch_ops;
  res_array

let check_linearizable () =
  let pool = Domainslib.Task.setup_pool ~num_domains:3 () in
  let res = Domainslib.Task.run pool (fun () -> 
      let bop_res = bop_exec pool batch |> Array.to_list in
      let seq_res = seq_exec batch in
      let itr = Iter.filter (fun exec -> 
          let bop_order = List.sort (fun (i1,_,_) (i2,_,_) 
                                      -> Int.compare i1 i2) exec in
          let cleaned_bop_order = List.map thrd bop_order in
          cleaned_bop_order = bop_res) seq_res in
      Iter.length itr, itr, bop_res
    ) in
  Domainslib.Task.teardown_pool pool;
  res

let () = 
  print_newline ();
  let linearizable, seq_res, bop_res = check_linearizable () in
  if linearizable > 0 then begin
    Format.printf "@[<v>@ A Sequential ordering was found for the batch!@ %a@ @\n (Valid Orderings)@]@." pp_batch_op_res (batch, bop_res);
    Iter.iter (fun res -> let seq_op_ordering = List.map snd res in
                let seq_res_ordering = List.map thrd res in
                Format.printf "@[<v>%a@ @]" pp_combine_list 
                  (seq_op_ordering, seq_res_ordering)) seq_res;
  end
  else 
    Format.printf "@[<v>@ No Sequential ordering exists for the batch!@ %a@ @ @]@." pp_batch_op_res (batch,bop_res)
