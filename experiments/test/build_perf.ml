module T = Domainslib.Task
let insert_size = (try Sys.argv.(1) with _  -> "10_000_000") |> int_of_string
let max_keys = (try Sys.argv.(2) with _  -> "3") |> int_of_string
let num_domains = ((try Sys.argv.(3) with _  -> "7") |> int_of_string) - 1
let test_seq_insert _pool () =
  let t = Btree.create ~max_keys () in
  for i = 0 to insert_size do
    Btree.insert t i i
  done;
  t

let test_build pool () =
  let elems = Array.init insert_size (fun i -> i,i) in
  let t = Btree.build 4 pool elems in
  t

let () = 
  Format.printf "Sequential build: ";
  Perf_utils.run test_seq_insert num_domains;
  print_newline ();
  Format.printf "Batch rebuild: ";
  Perf_utils.run test_build num_domains