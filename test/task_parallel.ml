open QCheck
open Domainslib

(** Property-based QCheck tests of Task.parallel_* *)

let count = 250

let test_parallel_for =
  Test.make ~name:"Domainslib.Task.parallel_for test" ~count
    (triple (int_bound 10) small_nat small_nat)
    (fun (num_domains,array_size,chunk_size) ->
       let pool = Task.setup_pool ~num_domains () in
       let res = Task.run pool (fun () ->
           let a = Atomic.make 0 in
           Task.parallel_for ~chunk_size ~start:0 ~finish:(array_size-1) ~body:(fun _ -> Atomic.incr a) pool;
           Atomic.get a) in
       Task.teardown_pool pool;
       res = array_size)

let test_parallel_for_reduce =
  Test.make ~name:"Domainslib.Task.parallel_for_reduce test" ~count
    (triple (int_bound 10) small_nat small_nat)
    (fun (num_domains,array_size,chunk_size) ->
       let pool = Task.setup_pool ~num_domains () in
       let res = Task.run pool (fun () ->
           Task.parallel_for_reduce ~chunk_size ~start:0 ~finish:(array_size-1) ~body:(fun _ -> 1) pool (+) 0) in
       Task.teardown_pool pool;
       res = array_size)

let test_parallel_scan =
  Test.make ~name:"Domainslib.Task.parallel_scan test" ~count
    (pair (int_bound 10) small_nat)
    (fun (num_domains,array_size) ->
       let pool = Task.setup_pool ~num_domains () in
       let a = Task.run pool (fun () -> Task.parallel_scan pool (+) (Array.make array_size 1)) in
       Task.teardown_pool pool;
       a = Array.init array_size (fun i -> i + 1))

let () =
  QCheck_base_runner.run_tests_main [
    test_parallel_for;
    test_parallel_for_reduce;
    test_parallel_scan;
  ]
