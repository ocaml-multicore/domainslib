let len = 1_000_000

let singleton_interval i = (i, i + 1)

let combine_intervals interval1 interval2  =
  let b1, e1 = interval1
  and b2, e2 = interval2 in
  if e1 <> b2 then begin
    Printf.eprintf "Invalid intervals: (%d, %d), (%d, %d)\n" b1 e1 b2 e2;
    assert false
  end
  else (b1, e2)

open Domainslib

let test_scan_ordering pool =
  let check_interval i interval =
    let (b, e) = interval in
    assert (b = 0 && e = i + 1)
  in
  Array.init len singleton_interval
  |> Task.parallel_scan pool combine_intervals
  |> Array.iteri check_interval

let () =
  (* [num_domains] is the number of *new* domains spawned by the pool
     performing computations in addition to the current domain. *)
  let num_domains = Domain.recommended_domain_count () - 1 in
  Printf.eprintf "test_parallel_scan on %d domains.\n" (num_domains + 1);
  let pool = Task.setup_pool ~num_domains ~name:"pool" () in
  Task.run pool begin fun () ->
    test_scan_ordering pool
  end;
  Task.teardown_pool pool;
  prerr_endline "Success.";