(* Despite what the name says, this test will not deadlock. A similar test will
 * deadlock in the version not using effect handlers. See
 * https://github.com/ocaml-multicore/ocaml-multicore/issues/670 *)

module T = Domainslib.Task

let n = try int_of_string Sys.argv.(1) with _ -> 1_000_000

let rec loop n =
  if n = 0 then
    Printf.printf "Looping finished on domain %d\n%!" (Domain.self () :> int)
  else (Domain.cpu_relax (); loop (n-1))

let () =
  let pool = T.setup_pool ~num_domains:2 () in
  T.run pool (fun _ ->
    let a = T.async pool (fun _ ->
      Printf.printf "Task A running on domain %d\n%!" (Domain.self () :> int);
      loop n)
    in
    let b = T.async pool (fun _ ->
      Printf.printf "Task B running on domain %d\n%!" (Domain.self () :> int);
      T.await pool a)
    in
    let c = T.async pool (fun _ ->
      Printf.printf "Task C running on domain %d\n%!" (Domain.self () :> int);
      T.await pool b)
    in
    loop n;
    T.await pool c);
  T.teardown_pool pool
