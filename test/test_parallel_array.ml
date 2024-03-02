(* Generic tests for the parray module *)

open Domainslib

let test_map_inplace pool = 
  let a = [| 1 ; 2 ; 3 ; 4 |] in
  Parray.map_inplace (fun x -> 2 * x) a pool;
  let res = [| 2 ; 4 ; 6 ; 8 |] in
  assert (a = res)

let test_mapi_inplace pool = 
  let a = [| 1 ; 2 ; 3 ; 4 |] in
  Parray.mapi_inplace (fun _ x -> 2 * x) a pool;
  let res = [| 2 ; 4 ; 6 ; 8 |] in
  assert (a = res)

let test_map pool = 
  let a = [| 1 ; 2 ; 3 ; 4 |] in
  let b = Parray.map (fun x -> 2 * x) a pool in
  let res = [| 2 ; 4 ; 6 ; 8 |] in
  assert (b = res)

let test_mapi pool = 
  let a = [| 1 ; 2 ; 3 ; 4 |] in
  let b = Parray.mapi (fun _ x -> 2 * x) a pool in
  let res = [| 2 ; 4 ; 6 ; 8 |] in
  assert (b = res)


let () =
  (* [num_domains] is the number of *new* domains spawned by the pool
     performing computations in addition to the current domain. *)
  let num_domains = Domain.recommended_domain_count () - 1 in
  Printf.eprintf "Test parray on %d domains.\n" (num_domains + 1);
  let pool = Task.setup_pool ~num_domains ~name:"pool" () in
  Task.run pool begin fun () ->
    test_map pool;
    test_map_inplace pool;
    test_mapi pool;
    test_mapi_inplace pool;
  end;
  Task.teardown_pool pool;
  prerr_endline "Success.";
