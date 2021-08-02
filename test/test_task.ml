(* Generic tests for the task module *)

(* Parallel for *)

open Domainslib
let modify_arr pool chunk_size = fun () ->
  let arr1 = Array.init 100 (fun i -> i + 1) in
  Task.parallel_for ~chunk_size ~start:0 ~finish:99
    ~body:(fun i -> arr1.(i) <- arr1.(i) * 2) pool;
  let arr_res = Array.init 100 (fun i -> (i + 1) * 2) in
  assert (arr1 = arr_res)

let inc_ctr pool chunk_size = fun () ->
  let ctr = Atomic.make 0 in
  Task.parallel_for ~chunk_size ~start:1 ~finish:1000
    ~body:(fun _ -> Atomic.incr ctr) pool;
  assert (Atomic.get ctr = 1000)

(* Parallel for reduce *)

let sum_sequence pool chunk_size init = fun () ->
  let v = Task.parallel_for_reduce ~chunk_size ~start:1
    ~finish:100 ~body:(fun i -> i) pool (+) init in
  assert (v = 5050 + init)

(* Parallel scan *)

let prefix_sum pool n = fun () ->
  let prefix_s l = List.rev (List.fold_left (fun a y -> match a with
    | [] -> [y]
    | x::_ -> (x+y)::a) [] l) in
  let arr = Array.make n 1 in
  let v1 = Task.parallel_scan pool (+) arr in
  let ls = Array.to_list arr in
  let v2 = prefix_s ls in
  assert (v1 = Array.of_list v2)

let run_all pool = fun () ->
  modify_arr pool 0 ();
  modify_arr pool 25 ();
  modify_arr pool 100 ();
  inc_ctr pool 0 ();
  inc_ctr pool 16 ();
  inc_ctr pool 32 ();
  inc_ctr pool 1000 ();
  sum_sequence pool 0 0 ();
  sum_sequence pool 10 10 ();
  sum_sequence pool 1 0 ();
  sum_sequence pool 1 10 ();
  sum_sequence pool 100 10 ();
  sum_sequence pool 100 100 ();
  prefix_sum pool 1000 ();
  prefix_sum pool 3 ()

let () =
  let pool = Task.setup_pool ~num_additional_domains:3 in
  run_all pool ();
  Task.teardown_pool pool;
  let pool2 = Task.setup_pool ~num_additional_domains:0 in
  run_all pool2 ();
  Task.teardown_pool pool2;
  let pool3 = Task.setup_pool ~num_additional_domains:31 in
  run_all pool3 ();
  Task.teardown_pool pool3;
  print_endline "ok"