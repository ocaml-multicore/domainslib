open Kcas
module T = Domainslib.Task

let var = Loc.make None

let () =
  let n = 100 in
  let pool_domain =
    Domain.spawn @@ fun () ->
    let pool =
      T.setup_pool ~num_domains:(Domain.recommended_domain_count () - 2) ()
    in
    T.run pool (fun () ->
        T.parallel_for ~start:1 ~finish:n
          ~body:(fun i ->
            ignore @@ Loc.update var
            @@ function None -> Some i | _ -> Retry.later ())
          pool);
    T.teardown_pool pool;
    Printf.printf "Done\n%!"
  in
  for _ = 1 to n do
    match
      Loc.update var @@ function None -> Retry.later () | Some _ -> None
    with
    | None -> failwith "impossible"
    | Some i -> Printf.printf "Got %d\n%!" i
  done;
  Domain.join pool_domain
