(* Try random elements 
   Insert a much larger batch size
*)

module T = Domainslib.Task

let max_rdm_int = (Int.shift_left 1 30) - 1
let max_children = (try Sys.argv.(1) with _  -> "3") |> int_of_string
let insert_size = (try Sys.argv.(2) with _  -> "1_000_000") |> int_of_string
let factor = (try Sys.argv.(3) with _ -> "10") |> int_of_string
let num_domains = ((try Sys.argv.(4) with _  -> "8") |> int_of_string) - 1
let preset_elems = Array.init insert_size 
    (fun _ -> let rdm = Random.int max_rdm_int in rdm, "key " ^ string_of_int rdm)
let[@warning "-32"] additional = Array.init (insert_size * factor)
    (fun _ -> let rdm = Random.int max_rdm_int in rdm, "key " ^ string_of_int rdm)
(* 
let additional = Array.init (insert_size * factor)
    (fun i -> i, "key" ^ string_of_int i) *)

let init () = 
  let t = Btree.create ~max_children () in
  Array.iter (fun (k,v) -> Btree.insert t k v) preset_elems;
  t

let[@warning "-32"] seq_insert t _pool () =
  Array.iter (fun (k,v) -> Btree.insert t k v) additional

let[@warning "-32"] rebuild_insert t pool () =
  Batch_para_btree.par_insert_rebuilder ~pool t additional

let () = 
  let t = init () in
  Format.printf "\n@[Sequential build: %.10fs@]@."
    (Perf_utils.run (seq_insert t) num_domains);
  let t = init () in
  Gc.full_major ();
  Format.printf "@[Batch rebuild: %.10fs@]@." 
    (Perf_utils.run (rebuild_insert t) num_domains)
