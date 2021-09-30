module T = Domainslib.Task
let num_domains = try int_of_string Sys.argv.(1) with _ -> 4
let n = try int_of_string Sys.argv.(2) with _ -> 100000

let gen n = Array.make n 1 (*(fun _ -> Random.int n)*)

let prefix_sum pool = T.parallel_scan pool (+)

let _ =
  let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in
  let arr = gen n in
  let t = Unix.gettimeofday() in
  let _ = prefix_sum pool arr in
  Printf.printf "Execution time: %fs\n" (Unix.gettimeofday() -. t);
  T.teardown_pool pool
