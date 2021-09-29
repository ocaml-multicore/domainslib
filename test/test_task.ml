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

let prefix_sum pool = fun () ->
  let prefix_s l = List.rev (List.fold_left (fun a y -> match a with
    | [] -> [y]
    | x::_ -> (x+y)::a) [] l) in
  let arr = Array.make 1000 1 in
  let v1 = Task.parallel_scan pool (+) arr in
  let ls = Array.to_list arr in
  let v2 = prefix_s ls in
  assert (v1 = Array.of_list v2)


let () =
  let pool1 = Task.setup_pool ~num_additional_domains:2 ~name:"pool1" () in
  let pool2 = Task.setup_pool ~num_additional_domains:2 ~name:"pool2" () in
  let p1 = Option.get @@ Task.lookup_pool "pool1" in
  modify_arr pool1 0 ();
  modify_arr pool1 25 ();
  modify_arr pool1 100 ();
  inc_ctr p1 0 ();
  inc_ctr p1 16 ();
  inc_ctr p1 32 ();
  inc_ctr p1 1000 ();
  let p2 = Option.get @@ Task.lookup_pool "pool2" in
  sum_sequence pool2 0 0 ();
  sum_sequence pool2 10 10 ();
  sum_sequence pool2 1 0 ();
  sum_sequence p2 1 10 ();
  sum_sequence p2 100 10 ();
  sum_sequence p2 100 100 ();
  prefix_sum p2 ();
  Task.teardown_pool pool1;
  Task.teardown_pool pool2;

  try
    sum_sequence pool2 0 0 ();
    assert false
  with Invalid_argument _ -> ();

  assert (Task.lookup_pool "pool1" = None);

  try
    let _ = Task.setup_pool ~num_additional_domains:(-1) () in ()
  with Invalid_argument _ -> ();
  print_endline "ok"
