let n = try int_of_string Sys.argv.(1) with _ -> 100
let num_domains = try Some (int_of_string Sys.argv.(2) - 1) with _ -> None

module T = Domainslib.Task

let _ =
  let p = T.setup_pool ?num_domains () in
  T.run p (fun _ ->
    T.parallel_for p ~start:0 ~finish:(n-1) ~chunk_size:16 ~body:(fun i ->
      print_string @@ Printf.sprintf "[%d] %d\n%!" (Domain.self () :> int) i));
  T.teardown_pool p
