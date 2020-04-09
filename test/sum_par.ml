let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 40

module T = Domainslib.Task_pool

let _ =
  let p = T.setup_pool ~num_domains:(num_domains - 1) in
  let ts = Unix.gettimeofday () in
  let sum =
    T.parallel_for_reduce p (+) 0 ~chunk_size:(n/(4*num_domains)) ~start:0
      ~finish:(n-1) ~body:(fun _i -> 1)
  in
  Printf.printf "Running time = %f seconds\n" (Unix.gettimeofday () -. ts);
  T.teardown_pool p;
  Printf.printf "Sum is %d\n" sum

