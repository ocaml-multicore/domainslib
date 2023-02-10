module T = Domainslib.Task


let size = 10_000_000
let elems = Array.init size (fun i -> i, "key" ^ string_of_int i)
(* let pool = T.setup_pool ~num_domains:(Domain.recommended_domain_count () - 1) ()*)
(* let t = T.run pool (fun () -> Batch_para_btree.build ~max_keys:3 pool elems) *)

let[@warning "-32"] seq_btree =
  let t = Btree.create () in
  Array.iter (fun (k, v) -> Btree.insert t k v) elems;
  Format.printf "Validate Seq Btree = %b" @@ Btree_validator.validate t

(* let test_correctness () = *)
(*   Array.for_all (fun (k,v) -> *)
(*       match Btree.search t k with *)
(*       | Some v' -> v' = v *)
(*       | None -> false ) elems *)

(* let () = *)
(*   let res = test_correctness () in *)
(*   T.teardown_pool pool; *)
(*   for _ = 0 to 1000 do *)
(*     let rdm = Random.int 1000000000 in *)
(*     Btree.insert t rdm "" *)
(*   done; *)
(*   assert(res) *)
