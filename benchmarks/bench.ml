[@@@alert "-unstable"]
module IntSet = Set.Make(Int)
module IntBtree = Data.Btree.Make(Int)
module BatchedIntBtree = Domainslib.Batcher.Make1(IntBtree)

module type BENCHMARK = sig

  type t
  (** [t] represents the data structure being bench-marked *)

  type test_spec
  (** [test] represents a specification for a particular test of the datastructure [t]. *)

  type spec_args
  (** [spec_args] represents any additional parameters that the specification expects *)

  val spec_args: spec_args Cmdliner.Term.t

  val test_spec: initial_count:int -> count:int -> min:int -> max:int -> spec_args -> test_spec
  (** [test_spec ~initial_count ~count ~min ~max] constructs a test for data structure t  *)

  val init: Domainslib.Task.pool -> test_spec -> t
  (** [init test_spec] constructs a new instance of the data structure from
      a given test. *)

  val run: Domainslib.Task.pool -> t -> test_spec -> unit
  (** [run pool t test] runs the test [test] on the data structure
      [t], using [pool] to schedule parallel tasks. *)

end

let benchmarks: (string, (module BENCHMARK)) Hashtbl.t = Hashtbl.create 32

let () =
  Hashtbl.add benchmarks "btree-sequential" (module Btree.Sequential);
  Hashtbl.add benchmarks "btree-coarse-grained" (module Btree.CoarseGrained);
  Hashtbl.add benchmarks "btree-batched" (module Btree.Batched);
  Hashtbl.add benchmarks "counter-sequential" (module Counter.Sequential);
  Hashtbl.add benchmarks "counter-coarse-grained" (module Counter.CoarseGrained);
  Hashtbl.add benchmarks "counter-batched" (module Counter.Batched);
  Hashtbl.add benchmarks "skiplist-sequential" (module Skiplist.Sequential);
  Hashtbl.add benchmarks "skiplist-coarse-grained" (module Skiplist.CoarseGrained);
  Hashtbl.add benchmarks "skiplist-batched" (module Skiplist.Batched)

let run_benchmark (type a) (module B: BENCHMARK with type spec_args = a)
    show_progress no_domains no_warmup no_iter 
    initial_count count min max (args: a) =
  let num_domains = match no_domains with None -> Domain.recommended_domain_count () | Some d -> d in
  let pool = Domainslib.Task.setup_pool ~num_domains () in
  let test = B.test_spec ~initial_count ~count ~min ~max args in
  Domainslib.Task.run pool (fun () ->
      Timing.time ~show_progress ~no_warmup ~no_iter ~init:(fun () -> B.init pool test ) (fun t ->
          B.run pool t test
        )
    )


let () =
  let open Cmdliner in
  let info' = Cmd.info ~doc:"Run benchmarks and print result times." "bench.exe" in

  let show_progress =
    Arg.(value @@ flag @@ info ~doc:"Whether to show progress bars" ["S"; "show-progress"]) in
  let no_domains =
    Arg.(value @@ opt (some int) None @@ info ~doc:"Number of domains to use" ["D"; "no-domains"]) in
  let no_warmup =
    Arg.(value @@ opt int 10 @@ info ~doc:"Number of warmup iterations to run" ["w"; "no-warmup"]) in
  let no_iter =
    Arg.(required @@ opt (some int) None @@ info ~doc:"Number of iterations to run" ["i"; "no-iter"]) in
  let initial_count =
    Arg.(value @@ opt int 0 @@ info ~doc:"Initial number of operations" ["init-count"]) in
  let count =
    Arg.(required @@ opt (some int) None @@ info ~doc:"Number of operations to run" ["c"; "count"]) in
  let min =
    Arg.(value @@ opt int 0 @@ info ~doc:"Minimum value of data for random inputs" ["min"]) in
  let max =
    Arg.(value @@ opt int 100000 @@ info ~doc:"Maximum value of data for random inputs" ["max"]) in

  let sub_cmds =
    Hashtbl.to_seq benchmarks
    |> Seq.map (fun (name, (module B: BENCHMARK)) ->
        let (b: (module BENCHMARK with type spec_args = B.spec_args)) = (module B) in
        let info = Cmd.info name in
        let action =
          Term.(const (run_benchmark b)
                $ show_progress $ no_domains $ no_warmup
                $ no_iter $ initial_count $ count $ min $ max
                $  B.spec_args) in
        Cmd.v info action
      )
    |> List.of_seq in

  exit (Cmd.eval (Cmd.group info' sub_cmds))
