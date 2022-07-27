open Domainslib

let print_array a =
  let b = Buffer.create 25 in
  Buffer.add_string b "[|";
  Array.iter (fun elem -> Buffer.add_string b (string_of_int elem ^ "; ")) a;
  Buffer.add_string b "|]";
  Buffer.contents b

let r = Array.init 20 (fun i -> i + 1)

let scan_task num_doms =
  let pool = Task.setup_pool ~num_domains:num_doms () in
  let a = Task.run pool (fun () -> Task.parallel_scan pool (+) (Array.make 20 1)) in
  Task.teardown_pool pool;
  Printf.printf "%i:  %s\n%!" num_doms (print_array a);
  assert (a = r)
;;
for num_dom=0 to 21 do
  scan_task num_dom;
done
