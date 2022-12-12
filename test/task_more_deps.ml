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

(* Generates a DAG of dependencies                          *)
(* Each task is represented by an array index w/a deps.list *)
(* This example DAG

     A/0 <--- B/1 <
      ^.           \
        \           \
         `- C/2 <--- D/3

   is represented as: [| []; [0]; [0]; [1;2] |] *)
let gen_dag n st =
  Array.init n (fun i ->
      let deps = ref [] in
      for dep = 0 to i-1 do
        if Gen.bool st then deps := dep :: !deps
      done;
      List.rev !deps)

type test_input =
  {
    num_domains  : int;
    length       : int;
    dependencies : int list array
  }

let show_test_input t =
  Printf.sprintf
    "{ num_domains : %i\n  length : %i\n  dependencies : %s }"
    t.num_domains t.length Print.(array (list int) t.dependencies)

let shrink_deps test_input =
  let ls = Array.to_list test_input.dependencies in
  let is = Shrink.list ~shrink:Shrink.list ls in
  Iter.map
    (fun deps ->
       let len = List.length deps in
       let arr = Array.of_list deps in
       let deps = Array.mapi (fun i i_deps -> match i,i_deps with
           | 0, _
           | _,[] -> []
           | _,[0] -> [0]
           | _, _ ->
             List.map (fun j ->
                 if j<0 || j>=len || j>=i (* ensure reduced dep is valid *)
                 then ((j + i) mod i)
                 else j) i_deps) arr in
       { test_input with length=len; dependencies=deps }) is

let arb_deps domain_bound promise_bound =
  let gen_deps =
    Gen.(pair (int_bound (domain_bound-1)) (int_bound promise_bound) >>= fun (num_domains,length) ->
         let num_domains = succ num_domains in
         let length = succ length in
         gen_dag length >>= fun dependencies -> return { num_domains; length; dependencies }) in
  make ~print:show_test_input ~shrink:(shrink_deps) gen_deps

let build_dep_graph pool test_input =
  let len = test_input.length in
  let deps = test_input.dependencies in
  let rec build i promise_acc =
    if i=len
    then promise_acc
    else
      let p = (match deps.(i) with
          | [] ->
            Task.async pool work
          | deps ->
            Task.async pool (fun () ->
                work ();
                List.iter (fun dep -> Task.await pool (List.nth promise_acc (i-1-dep))) deps)) in
      build (i+1) (p::promise_acc)
  in
  build 0 []

let test_one_pool ~domain_bound ~promise_bound =
  Test.make ~name:"Domainslib.Task.async/await, more deps, 1 work pool" ~count:100
    (arb_deps domain_bound promise_bound)
    (Util.repeat 10
       (fun test_input ->
          let pool = Task.setup_pool ~num_domains:test_input.num_domains () in
          Task.run pool (fun () ->
              let ps = build_dep_graph pool test_input in
              List.iter (fun p -> Task.await pool p) ps);
          Task.teardown_pool pool;
          true))

let () =
  QCheck_base_runner.run_tests_main [test_one_pool ~domain_bound:8 ~promise_bound:10]
