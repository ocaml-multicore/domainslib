module T = Domainslib.Task
let num_domains = try int_of_string Sys.argv.(1) with _ -> 4
let n = try int_of_string Sys.argv.(2) with _ -> 100000

let gen n = Array.make n 1 (*(fun _ -> Random.int n)*)

let prefix_sum = T.parallel_scan (+)

let _ =
  let arr = gen n in
  let t = Unix.gettimeofday() in
  let _ = prefix_sum arr in
  Printf.printf "Execution time: %fs\n" (Unix.gettimeofday() -. t);
  T.Pool.teardown_default_pool
