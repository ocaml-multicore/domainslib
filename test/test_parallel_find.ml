let len = 1_000_000
let nb_needles = 4

let () = Random.init 42

let needles =
  Array.init nb_needles (fun _ -> Random.int len)

let input =
  let t = Array.make len false in
  needles |> Array.iter (fun needle ->
    t.(needle) <- true
  );
  t

open Domainslib

let search_needle pool ~chunk_size =
  Task.parallel_find pool ~chunk_size ~start:0 ~finish:(len - 1) ~body:(fun i ->
    if input.(i) then Some i
    else None
  )

let test_search pool ~chunk_size =
  match search_needle pool ~chunk_size with
  | None -> assert false
  | Some needle ->
    assert (Array.exists ((=) needle) needles)

let () =
  (* [num_domains] is the number of *new* domains spawned by the pool
     performing computations in addition to the current domain. *)
  let num_domains = Domain.recommended_domain_count () - 1 in
  Printf.eprintf "test_parallel_find on %d domains.\n" (num_domains + 1);
  let pool = Task.setup_pool ~num_domains ~name:"pool" () in
  Task.run pool begin fun () ->
    [0; 16; 32; 1000] |> List.iter (fun chunk_size ->
      test_search pool ~chunk_size)
  end;
  Task.teardown_pool pool;
  prerr_endline "Success.";
