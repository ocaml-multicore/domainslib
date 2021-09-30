let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 100

module T = Domainslib.Task

let _ =
  let p = T.setup_pool ~num_additional_domains:(num_domains - 1) () in
  T.parallel_for p ~start:0 ~finish:(n-1) ~chunk_size:16 ~body:(fun i ->
    print_string @@ Printf.sprintf "[%d] %d\n%!" (Domain.self () :> int) i);
  T.teardown_pool p
