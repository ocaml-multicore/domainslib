(* Generic tests for the task module *)

(* Parallel for *)

open Domainslib
let modify_arr  chunk_size = fun () ->
  let arr1 = Array.init 100 (fun i -> i + 1) in
  Task.parallel_for ~chunk_size ~start:0 ~finish:99
    ~body:(fun i -> arr1.(i) <- arr1.(i) * 2) ();
  let arr_res = Array.init 100 (fun i -> (i + 1) * 2) in
  assert (arr1 = arr_res)

let inc_ctr  chunk_size = fun () ->
  let ctr = Atomic.make 0 in
  Task.parallel_for ~chunk_size ~start:1 ~finish:1000
    ~body:(fun _ -> Atomic.incr ctr) ();
  assert (Atomic.get ctr = 1000)

(* Parallel for reduce *)

let sum_sequence  chunk_size init = fun () ->
  let v = Task.parallel_for_reduce ~chunk_size ~start:1
    ~finish:100 ~body:(fun i -> i)  (+) init in
  assert (v = 5050 + init)

(* Parallel scan *)

let prefix_sum  = fun () ->
  let prefix_s l = List.rev (List.fold_left (fun a y -> match a with
    | [] -> [y]
    | x::_ -> (x+y)::a) [] l) in
  let arr = Array.make 1000 1 in
  let v1 = Task.parallel_scan  (+) arr in
  let ls = Array.to_list arr in
  let v2 = prefix_s ls in
  assert (v1 = Array.of_list v2)


let () =
  modify_arr  0 ();
  modify_arr  25 ();
  modify_arr  100 ();
  inc_ctr  0 ();
  inc_ctr  16 ();
  inc_ctr  32 ();
  inc_ctr  1000 ();
  sum_sequence  0 0 ();
  sum_sequence  10 10 ();
  sum_sequence  1 0 ();
  sum_sequence  1 10 ();
  sum_sequence  100 10 ();
  sum_sequence  100 100 ();
  prefix_sum  ();
  Task.Pool.teardown_default_pool ();
  print_endline "ok"