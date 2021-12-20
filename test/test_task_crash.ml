open Domainslib

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  for _ = 1 to 200 do
    assert (7 = tak 18 12 6);
  done
;;
begin
  let pool1 = Task.setup_pool ~num_additional_domains:2 () in
  let pool2 = Task.setup_pool ~num_additional_domains:1 () in

  let pool1_prom0 = Task.async pool1 work in

  let pool2_prom0 = Task.async pool2 work in
  let pool2_prom1 = Task.async pool2 work in

  Task.run pool1 (fun () -> List.iter (fun p -> Task.await pool1 p) [pool1_prom0]);
  Task.run pool2 (fun () -> List.iter (fun p -> Task.await pool2 p) [pool2_prom0; pool2_prom1]);

  Task.teardown_pool pool1;
  Task.teardown_pool pool2;
end
