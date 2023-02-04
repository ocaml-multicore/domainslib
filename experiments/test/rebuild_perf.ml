module T = Domainslib.Task
let insert_size = (try Sys.argv.(1) with _  -> "10_000_000") |> int_of_string
let max_keys = (try Sys.argv.(2) with _  -> "3") |> int_of_string
let num_domains = ((try Sys.argv.(3) with _  -> "7") |> int_of_string) - 1
let elems = Array.init insert_size 
    (fun i -> let even = i*2 in even, "key " ^ string_of_int even)
let additional =  Array.init insert_size (fun i -> 
    let odd = (i*2)+1 in odd, "key " ^ string_of_int odd)

let init () = 
  let t = Btree.create ~max_keys () in
  Array.iter (fun (k,v) -> Btree.insert t k v) elems;
  t

let seq_insert t _pool () =
  Array.iter (fun (k,v) -> Btree.insert t k v) additional

let rebuild_insert t pool () =
  Batch_para_btree.par_insert_rebuilder ~pool t additional

let () = 
  let t = init () in
  Format.printf "\n@[Sequential build: %.2fs@]@."
    (Perf_utils.run (seq_insert t) num_domains);
  let t = init () in
  Gc.full_major ();
  Format.printf "@[Batch rebuild: %.2fs@]@." 
    (Perf_utils.run (rebuild_insert t) num_domains)