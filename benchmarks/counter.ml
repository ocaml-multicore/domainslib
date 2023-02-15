module Counter = Data.Counter.Sequential
module BatchedCounter = Domainslib.Batcher.Make(Data.Counter)

type generic_test_spec = {
  increments : int;
  decrements : int;
  gets : int
}

type generic_spec_args = {
  random_op : bool
}

let generic_spec_args: generic_spec_args Cmdliner.Term.t =
  let open Cmdliner in
  let rand = Arg.(value @@ flag @@ info ~doc:"Random operations" ["rand"]) in
  Term.(const (fun rand -> {random_op = rand}) $ rand )

let generic_test_spec ~count spec_args = 
  if spec_args.random_op then begin
    let i, d, g = ref 0, ref 0, ref 0 in
    for _ = 1 to count do
      match Random.int 2 with
      | 0 -> incr i
      | 1 -> incr d
      | 2 -> incr g
      | _ -> failwith "Impossible"
    done;
    {increments = !i; decrements = !d; gets = !g}
  end
  else
    {increments = count; decrements = 0; gets = 0}

module Sequential = struct

  type t = Counter.t

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~initial_count ~count ~min ~max spec_args =
    let _, _ , _ = initial_count, min, max in
    generic_test_spec ~count spec_args

  let init _pool _test_spec = Counter.init ()
  let run _pool t test_spec =
    for _ = 1 to test_spec.increments do
      Counter.incr t
    done;
    for _ = 1 to test_spec.decrements do
      Counter.decr t
    done;
    for _ = 1 to test_spec.gets do
      ignore @@ Counter.get t
    done
end


module CoarseGrained = struct

  type t = { mutable counter : Counter.t; mutex : Mutex.t}

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~initial_count ~count ~min ~max spec_args =
    let _, _ , _ = initial_count, min, max in
    generic_test_spec ~count spec_args

  let init _pool _test_spec = 
    {counter = Counter.init (); mutex = Mutex.create ()}

  let run pool t test_spec =
    let total = test_spec.increments + test_spec.decrements + test_spec.gets in
    Domainslib.Task.parallel_for pool
      ~start:1 ~finish:total
      ~body:(fun i ->
          Mutex.lock t.mutex;
          Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
              if i < test_spec.increments 
              then Counter.incr t.counter
              else if i < test_spec.increments + test_spec.decrements 
              then Counter.decr t.counter
              else Counter.get t.counter |> ignore
            )
        )
end

module Batched = struct

  type t = BatchedCounter.t

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~initial_count ~count ~min ~max spec_args =
    let _, _ , _ = initial_count, min, max in
    generic_test_spec ~count spec_args

  let init pool _test_spec = BatchedCounter.init pool

  let run pool (counter: t) test_spec =
    let total = test_spec.increments + test_spec.decrements + test_spec.gets in
    Domainslib.Task.parallel_for pool
      ~start:1 ~finish:total
      ~body:(fun i ->
          if i < test_spec.increments 
          then BatchedCounter.apply counter Incr
          else if i < test_spec.increments + test_spec.decrements 
          then BatchedCounter.apply counter Decr
          else BatchedCounter.apply counter Get |> ignore
        )

end

