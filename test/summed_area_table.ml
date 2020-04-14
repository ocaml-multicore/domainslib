module T = Domainslib.Task
let num_domains = try int_of_string Sys.argv.(1) with _ -> 4
let size = try int_of_string Sys.argv.(2) with _ -> 100

let transpose a =
  let r = Array.length a in
  let c = Array.length a.(0) in
  let b = Array.copy a in
  for i = 0 to (pred r) do
    for j = 0 to (pred c) do
      b.(j).(i) <- a.(i).(j)
    done
  done;
  b

let calc_table pool mat =
  let l = Array.length mat in
  let res = Array.copy mat in
  for i = 0 to (l - 1) do
    res.(i) <- T.parallel_scan pool (fun x y -> x + y) mat.(i)
  done;
  let k = transpose res in

  for i = 0 to (l - 1) do
    res.(i) <- T.parallel_scan pool (fun x y -> x + y) k.(i)
  done;
  (transpose res)

let _ =
  let m = Array.init size (fun _ -> Array.init size (fun _ -> Random.int size))
  in
  let pool = T.setup_pool ~num_domains:(num_domains - 1) in
  let _ = calc_table pool m in

  (* for i = 0 to size-1 do
    for j = 0 to size-1 do
      print_int a.(i).(j); print_string "  "
    done;
    print_newline()
  done; *)
  T.teardown_pool pool
