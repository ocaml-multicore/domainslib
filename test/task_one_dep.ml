(**
  Generate tests of async+await from Domainslib.Task.
  It does so by generating a random, acyclic dependency graph of [async] tasks,
  each [await]ing on its dependency.
 *)

open QCheck
open Domainslib

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  for _ = 1 to 200 do
    assert (7 = tak 18 12 6);
  done

(* Generates a sparse DAG of dependencies                           *)
(* Each task is represented by an array index w/at most 1 dep. each *)
(* This example DAG

     A/0 <--- B/1
      ^.
        \
         `- C/2 <--- D/3

   is represented as: [| None; Some 0; Some 0; Some 2 |] *)
let gen_deps n st =
  let a = Array.make n None in
  for i=1 to n-1 do
    if Gen.bool st then a.(i) <- Some (Gen.int_bound (i-1) st)
  done;
  a

type test_input =
  {
    num_domains  : int;
    length       : int;
    dependencies : int option array
  }

let show_test_input t =
  Printf.sprintf
    "{ num_domains : %i\n  length : %i\n  dependencies : %s }"
    t.num_domains t.length Print.(array (option int) t.dependencies)

let shrink_deps test_input =
  let ls = Array.to_list test_input.dependencies in
  let is = Shrink.list ~shrink:Shrink.(option nil) ls in
  Iter.map
    (fun deps ->
       let len = List.length deps in
       let arr = Array.of_list deps in
       let deps = Array.mapi (fun i j_opt -> match i,j_opt with
            | 0, _
            | _,None -> None
            | _,Some 0 -> Some 0
            | _, Some j ->
              if j<0 || j>=len || j>=i (* ensure reduced dep is valid *)
              then Some ((j + i) mod i)
              else Some j) arr in
       { test_input with length=len; dependencies=deps }) is

let arb_deps domain_bound promise_bound =
  let gen_deps =
    Gen.(pair (int_bound (domain_bound-1)) (int_bound promise_bound) >>= fun (num_domains,length) ->
         let num_domains = succ num_domains in
         let length = succ length in
         gen_deps length >>= fun dependencies -> return { num_domains; length; dependencies }) in
  let shrink_input input =
    Iter.append
      (Iter.map (fun doms' -> { input with num_domains = doms' }) (Shrink.int input.num_domains))
      (shrink_deps input) in
  make ~print:show_test_input ~shrink:shrink_input gen_deps

let build_dep_graph pool test_input =
  let len = test_input.length in
  let deps = test_input.dependencies in
  let rec build i promise_acc =
    if i=len
    then promise_acc
    else
      let p = (match deps.(i) with
          | None ->
            Task.async pool work
          | Some dep ->
            Task.async pool (fun () ->
                work();
                Task.await pool (List.nth promise_acc (i-1-dep)))) in
      build (i+1) (p::promise_acc)
  in
  build 0 []

let test_one_pool ~domain_bound ~promise_bound =
  Test.make ~name:"Domainslib.Task.async/await, one dep, 1 work pool" ~count:100
    (arb_deps domain_bound promise_bound)
    (Util.repeat 10 @@
     fun input ->
     let pool = Task.setup_pool ~num_domains:input.num_domains () in
     Task.run pool (fun () ->
         let ps = build_dep_graph pool input in
         List.iter (fun p -> Task.await pool p) ps);
     Task.teardown_pool pool;
     true)

let test_two_pools_sync_last ~domain_bound ~promise_bound =
  let gen = arb_deps domain_bound promise_bound in
  Test.make ~name:"Domainslib.Task.async/await, one dep, w.2 pools, syncing at the end" ~count:100
    (pair gen gen)
    (Util.repeat 10 @@
     fun (input1,input2) ->
     try
       let pool1 = Task.setup_pool ~num_domains:input1.num_domains () in
       let pool2 = Task.setup_pool ~num_domains:input2.num_domains () in
       let ps1 = build_dep_graph pool1 input1 in
       let ps2 = build_dep_graph pool2 input2 in
       Task.run pool1 (fun () -> List.iter (fun p -> Task.await pool1 p) ps1);
       Task.run pool2 (fun () -> List.iter (fun p -> Task.await pool2 p) ps2);
       Task.teardown_pool pool1;
       Task.teardown_pool pool2;
       true
     with
       Failure err -> err = "failed to allocate domain")

let test_two_nested_pools ~domain_bound ~promise_bound =
  let gen = arb_deps domain_bound promise_bound in
  Test.make ~name:"Domainslib.Task.async/await, one dep, w.2 nested pools" ~count:100
    (pair gen gen)
    (Util.repeat 10 @@
     fun (input1,input2) ->
     try
       let pool1 = Task.setup_pool ~num_domains:input1.num_domains () in
       let pool2 = Task.setup_pool ~num_domains:input2.num_domains () in
       Task.run pool1 (fun () ->
           Task.run pool2 (fun () ->
               let ps1 = build_dep_graph pool1 input1 in
               let ps2 = build_dep_graph pool2 input2 in
               List.iter (fun p -> Task.await pool1 p) ps1;
               List.iter (fun p -> Task.await pool2 p) ps2));
       Task.teardown_pool pool1;
       Task.teardown_pool pool2;
       true
     with
       Failure err -> err = "failed to allocate domain")

let () =
  let domain_bound = max 1 (Domain.recommended_domain_count () / 2) in
  let promise_bound = max 2 domain_bound in
  QCheck_base_runner.run_tests_main [
    test_one_pool            ~domain_bound ~promise_bound;
    test_two_pools_sync_last ~domain_bound ~promise_bound;
    test_two_nested_pools    ~domain_bound ~promise_bound;
  ]
