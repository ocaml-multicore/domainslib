(Batched) Multicore tests
===============

Property-based tests of Batched data structures, derived from [multicoretests](https://github.com/ocaml-multicore/multicoretests).

This project contains
- a randomized test suite of OCaml 5.0, packaged up in `multicoretests.opam`
- a reusable testing library for batched data structures `Lin_batched`.

All of the above build on [QCheck](https://github.com/c-cube/qcheck),
a black-box, property-based testing library in the style of
QuickCheck.

A Linearization Tester for Batched Data structures
==================================================

Like
[multicoretests](https://github.com/ocaml-multicore/multicoretests)'s
`Lin` module, the `Lin_batched` module lets a user test an batched API
for *sequential consistency*, i.e., it sends a sequence of random
commands to the batched API, records the results, and checks whether
the observed results can be linearized and reconciled with some
sequential execution. The library offers an embedded, combinator DSL
to describe signatures succinctly.


As an example, the required specification to test
the `Btree` module is as follows:

```ocaml
module IntBtree = Data.Btree.Make(Int)
module Btree : Spec = struct

  type t = int IntBtree.Sequential.t
  type wrapped_op = int IntBtree.wrapped_op
  let init : unit -> t = IntBtree.init
  let cleanup : t -> unit = fun _ -> ()

  let run : Domainslib.Task.pool -> t -> wrapped_op list -> unit =
    fun pool state ops ->
    let ops = Array.of_list ops in
    IntBtree.run state pool ops
    
  let api = [
    val_ "insert" IntBtree.Sequential.insert
      (fun set _ key vl -> IntBtree.(Mk (Insert (key,vl), set)))
      (t @-> int @-> int @-> returning unit);

    val_ "search" IntBtree.Sequential.search
      (fun set _ key -> IntBtree.(Mk (Search key, set)))
      (t @-> int @-> returning (option int));

    val_ "size" IntBtree.Sequential.size
      (fun set _ -> IntBtree.(Mk (Size, set)))
      (t @-> returning int);
  ]

end

module BTT = Lin_batched.Make(Btree)
QCheck_base_runner.run_tests_main [
  BTT.lin_test ~count:100 ~name:"Btree test"
]
```


``` ocaml
module HashtblSig =
struct
  type t = (char, int) Hashtbl.t
  let init () = Hashtbl.create ~random:false 42
  let cleanup _ = ()

  open Lin
  let a,b = char_printable,nat_small
  let api =
    [ val_ "Hashtbl.add"    Hashtbl.add    (t @-> a @-> b @-> returning unit);
      val_ "Hashtbl.remove" Hashtbl.remove (t @-> a @-> returning unit);
      val_ "Hashtbl.find"   Hashtbl.find   (t @-> a @-> returning_or_exc b);
      val_ "Hashtbl.mem"    Hashtbl.mem    (t @-> a @-> returning bool);
      val_ "Hashtbl.length" Hashtbl.length (t @-> returning int); ]
end

module HT = Lin_domain.Make(HashtblSig)
;;
QCheck_base_runner.run_tests_main [
  HT.lin_test `Domain ~count:1000 ~name:"Hashtbl DSL test";
]
```

The first line indicates the type of the system under test along with
bindings `init` and `cleanup` for setting it up and tearing it down.
The `api` then contains a list of type signature descriptions using
combinators `unit`, `bool`, `int`, `returning`, `returning_or_exc`,
... in the style of [Ctypes](https://github.com/ocamllabs/ocaml-ctypes).
The functor `Lin_domain.Make` expects a description of the tested
commands and outputs a module with a QCheck test `lin_test` that
performs the linearization test.

The QCheck linearization test iterates a number of test
instances. Each instance consists of a "sequential prefix" of calls to
the above commands, followed by a `spawn` of two parallel `Domain`s
that each call a sequence of operations. `Lin` chooses the individual
operations and arguments arbitrarily and records their results. The
framework then performs a search for a sequential interleaving of the
same calls, and succeeds if it finds one.

Since `Hashtbl`s are not safe for parallelism, if you run
`dune exec doc/example/lin_tests_dsl.exe` the output can produce the
following output, where each tested command is annotated with its result:
```

Messages for test Hashtbl DSL test:

  Results incompatible with sequential execution

                                    |
                                    |
                 .------------------------------------.
                 |                                    |
     Hashtbl.add t 'a' 0  : ()            Hashtbl.add t 'a' 0  : ()
       Hashtbl.length t  : 1                Hashtbl.length t  : 1
```

In this case, the test tells us that there is no sequential
interleaving of these calls which would return `1` from both calls to
`Hashtbl.length`. For example, in the following sequential interleaving
the last call should return `2`:
``` ocaml
 Hashtbl.add t 'a' 0;;
 let res1 = Hashtbl.length t;;
 Hashtbl.add t 'a' 0;;
 let res2 = Hashtbl.length t;;
```

See [src/atomic/lin_tests_dsl.ml](src/atomic/lin_tests_dsl.ml) for
another example of testing the `Atomic` module.


A Parallel State-Machine Testing Library
========================================

`STM` contains a revision of [qcstm](https://github.com/jmid/qcstm)
extended to run parallel state-machine tests akin to [Erlang
QuickCheck, Haskell Hedgehog, ScalaCheck, ...](https://github.com/jmid/pbt-frameworks).
To do so, the `STM` library also performs a sequence of random
operations in parallel and records the results. In contrast to `Lin`,
`STM` then checks whether the observed results are linearizable by
reconciling them with a sequential execution of a `model` description.
The `model` expresses the intended meaning of each tested command. As
such, it requires more of the user compared to `Lin`. The
corresponding code to describe a `Hashtbl` test using `STM` is
given below:

``` ocaml
open QCheck
open STM

(** parallel STM tests of Hashtbl *)

module HashtblModel =
struct
  type sut = (char, int) Hashtbl.t
  type state = (char * int) list
  type cmd =
    | Add of char * int
    | Remove of char
    | Find of char
    | Mem of char
    | Length [@@deriving show { with_path = false }]

  let init_sut () = Hashtbl.create ~random:false 42
  let cleanup (_:sut) = ()

  let arb_cmd (s:state) =
    let char =
      if s=[]
      then Gen.printable
      else Gen.(oneof [oneofl (List.map fst s); printable]) in
    let int = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof
         [Gen.map2 (fun k v -> Add (k,v)) char int;
          Gen.map  (fun k   -> Remove k) char;
          Gen.map  (fun k   -> Find k) char;
          Gen.map  (fun k   -> Mem k) char;
          Gen.return Length; ])

  let next_state (c:cmd) (s:state) = match c with
    | Add (k,v) -> (k,v)::s
    | Remove k  -> List.remove_assoc k s
    | Find _
    | Mem _
    | Length    -> s

  let run (c:cmd) (h:sut) = match c with
    | Add (k,v) -> Res (unit, Hashtbl.add h k v)
    | Remove k  -> Res (unit, Hashtbl.remove h k)
    | Find k    -> Res (result int exn, protect (Hashtbl.find h) k)
    | Mem k     -> Res (bool, Hashtbl.mem h k)
    | Length    -> Res (int,  Hashtbl.length h)

  let init_state = []

  let precond (_:cmd) (_:state) = true
  let postcond (c:cmd) (s:state) (res:res) = match c,res with
    | Add (_,_), Res ((Unit,_),_)
    | Remove _,  Res ((Unit,_),_) -> true
    | Find k,    Res ((Result (Int,Exn),_),r) -> r = (try Ok (List.assoc k s) with Not_found -> Error Not_found)
    | Mem k,     Res ((Bool,_),r) -> r = List.mem_assoc k s
    | Length,    Res ((Int,_),r)  -> r = List.length s
    | _ -> false
end

module HT_seq = STM_sequential.Make(HashtblModel)
module HT_dom = STM_domain.Make(HashtblModel)
;;
QCheck_base_runner.run_tests_main
  (let count = 200 in
   [HT_seq.agree_test     ~count ~name:"Hashtbl test sequential";
    HT_dom.agree_test_par ~count ~name:"Hashtbl test parallel"; ])
```

Again this requires a type `sut` for the system under test, and
bindings `init_sut` and `cleanup` for setting it up and tearing it
down. The type `cmd` describes the tested commands.

The type `state = (char * int) list` describes with a pure association
list the internal state of a `Hashtbl`. The `init_state` represents
the empty `Hashtbl` mode and the state transition function
`next_state` describes how the it changes across each `cmd`. For
example, `Add (k,v)` appends the key-value pair onto the association
list.

`arb_cmd` is a generator of `cmd`s, taking `state` as a parameter.
This allows for `state`-dependent `cmd` generation, which we use
to increase the chance of producing a `Remove 'c'`, `Find 'c'`, ...
following an `Add 'c'`. Internally `arb_cmd` uses QCheck combinators
`Gen.return`, `Gen.map`, and `Gen.map2` to generate one of
5 different commands.

`run` executes the tested `cmd` over the `sut` and wraps the result up
in a result type `res` offered by `STM`. Combinators `unit`, `bool`,
`int`, ... allow to annotate the result with the expected type.
`postcond` expresses a post-condition by matching the received `res`,
for a `cmd` with the corresponding answer from the `model`. For
example, this compares the Boolean result `r` from `Hashtbl.mem` with
the result from `List.mem_assoc`. Similarly `precond` expresses a
pre-condition.

The module is phrased as functors:
 - the functor `STM_sequential.Make` produces a module with a function
   `agree_test` to test whether the model agrees with the `sut` across
   a sequential run of an arbitrary command sequence and
 - the functor `STM_domain.Make` produces a module with a function
   `agree_test_par` which tests in parallel by `spawn`ing two domains
   with `Domain` similarly to `Lin` and searches for a sequential
   interleaving over the model.

When running the above with the command `dune exec doc/example/stm_tests.exe`
one may obtain the following output:
```
Messages for test Hashtbl test parallel:

  Results incompatible with linearized model

                                  |
                                  |
               .------------------------------------.
               |                                    |
     (Add ('e', 5268)) : ()                (Add ('!', 4)) : ()
           Length : 1                           Length : 1
```

This illustrates how two hashtable `Add` commands may interfere when
executed in parallel, leaving only `1` entry in the resulting
`Hashtbl` - which is not reconcilable with the declarative model
description.


The above examples are available from the [doc/example](doc/example)
directory. The [doc](doc) directory also contains a recent paper
presenting the project in a bit more detail.



Repeatability Efforts
=====================

Both `Lin` and `STM` perform randomized property-based testing with
QCheck. When rerunning a test to shrink/reduce the test input, QCheck
thus starts from the same `Random` seed to limit non-determinism.
This is however not suffient for multicore programs where CPU
scheduling and garbage collection may hinder reproducibility.

`Lin` and `STM` primarily uses test repetition to increase
reproducibility and it is sufficient that only a single repetition
triggers an issue. Currently repeating a non-deterministic QCheck
property can be done in two different ways:
 - a `repeat`-combinator lets you test a property, e.g., 50 times
   rather than just 1. (Pro: a failure is found faster, Con: wasted,
   repetitive testing when there are no failures)
 - [a recent `QCheck` PR](https://github.com/c-cube/qcheck/pull/212) extends `Test.make` with a `~retries` parameter causing
   it to only perform repetition during shrinking. (Pro: each test is
   cheaper so we can run more, Con: more tests are required to trigger a race)


Issues
======

Parallel `Weak` `Hashset` usage may crash the runtime (new)
-----------------------------------------------------------

Parallel `STM` tests found a combination of `Weak` `Hashset` functions
that [may cause the run-time to `abort` or segfault](https://github.com/ocaml/ocaml/issues/11934)


`Sys.readdir` on MingW disagrees with Linux behavior (new, fixed)
-----------------------------------------------------------------

Sequential `STM` tests of `Sys` showed how `Sys.readdir` of a
non-existing directory on MingW Windows [returns an empty `array`, thus
disagreeing with the Linux and macOS behavior](https://github.com/ocaml/ocaml/issues/11829)


`seek` on a closed `in_channel` may read uninitialized memory (new)
-------------------------------------------------------------------

A failure of `Lin` `In_channel` tests revealed that `seek` on a closed
`in_channel` [may read uninitialized memory](https://github.com/ocaml/ocaml/issues/11878)


Parallel usage of `Weak` could produce weird values (new, fixed)
----------------------------------------------------------------

Racing `Weak.set` and `Weak.get` [can in some cases produce strange values](https://github.com/ocaml/ocaml/pull/11749)


Bytecode interpreter can segfault on unhandled `Effect` (new, fixed)
--------------------------------------------------------------------

In seldom cases the tests would [trigger a segfault in the bytecode interpreter when treating an unhandled `Effect`](https://github.com/ocaml/ocaml/issues/11669)


`Ephemeron` can fail assert and abort (new, fixed)
-------------------------------------------------

In some cases (even sequential) [the `Ephemeron` tests can trigger an assertion failure and abort](https://github.com/ocaml/ocaml/issues/11503).


Parallel usage of `Bytes.escaped` is unsafe (new, fixed)
--------------------------------------------------------

The `Bytes` tests triggered a segfault which turned out to be caused by [an unsafe `Bytes.escaped` definition](https://github.com/ocaml/ocaml/issues/11508).


Infinite loop in `caml_scan_stack` on ARM64 (known, fixed)
----------------------------------------------------------

The tests triggered [an apparent infinite loop on ARM64 while amd64 would complete the tests as expected](https://github.com/ocaml/ocaml/issues/11425).


Unsafe `Buffer` module (new, fixed)
-----------------------------------

The tests found that the `Buffer` module implementation is [unsafe under parallel usage](https://github.com/ocaml/ocaml/issues/11279) - initially described in [multicoretests#63](https://github.com/ocaml-multicore/multicoretests/pull/63).


MacOS segfault (new, fixed)
---------------------------

The tests found an issue causing [a segfault on MacOS](https://github.com/ocaml/ocaml/issues/11226).


`In_channel` and `Out_channel` unsafety (new, status?)
------------------------------------------------------

The tests found a problem with `In_channel` and `Out_channel` which
could trigger segfaults under parallel usage. For details see
[issue ocaml-multicore/multicoretests#13](https://github.com/ocaml-multicore/multicoretests/pull/13) and
[this ocaml/ocaml#10960 comment](https://github.com/ocaml/ocaml/issues/10960#issuecomment-1087660763).


Cornercase issue in `Domainslib` (new, fixed)
---------------------------------------------

The tests found an issue in `Domainslib.parallel_for_reduce` which
[would yield the wrong result for empty arrays](https://github.com/ocaml-multicore/domainslib/pull/67).
As of [domainslib#100](https://github.com/ocaml-multicore/domainslib/pull/100)
the `Domainslib` tests have been moved to the `Domainslib` repo.


Specification of `Lockfree.Ws_deque` (fixed)
--------------------------------------------

The initial tests of `ws_deque` just applied the parallelism property `agree_prop_par`.
However that is not sufficient, as only the original domain (thread)
is allowed to call `push`, `pop`, ..., while a `spawn`ed domain
should call only `steal`.

A custom, revised property test runs a `cmd` prefix, then
`spawn`s a "stealer domain" with `steal`, ... calls, while the
original domain performs calls across a broder random selection
(`push`, `pop`, ...). As of
[lockfree#43](https://github.com/ocaml-multicore/lockfree/pull/43)
this test has now been moved to the `lockfree` repo.

Here is an example output illustrating how `size` may return `-1` when
used in a "stealer domain". The first line in the `Failure` section lists
the original domain's commands and the second lists the stealer
domains commands (`Steal`,...). The second `Messages` section lists a
rough dump of the corresponding return values: `RSteal (Some 73)` is
the result of `Steal`, ... Here it is clear that the spawned domain
successfully steals 73, and then observes both a `-1` and `0` result from
`size` depending on timing. `Size` should therefore not be considered
threadsafe (none of the
[two](https://www.dre.vanderbilt.edu/~schmidt/PDF/work-stealing-dequeue.pdf)
[papers](https://hal.inria.fr/hal-00802885/document) make any such
promises though):

``` ocaml
$ dune exec src/ws_deque_test.exe
random seed: 55610855
generated error  fail  pass / total     time test name
[✗]   318     0     1   317 / 10000     2.4s parallel ws_deque test (w/repeat)

--- Failure --------------------------------------------------------------------

Test parallel ws_deque test (w/repeat) failed (8 shrink steps):

 Seq.prefix:  Parallel procs.:

          []  [(Push 73); Pop; Is_empty; Size]

              [Steal; Size; Size]


+++ Messages ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Messages for test parallel ws_deque test (w/repeat):

Result observations not explainable by linearized model:

  Seq.prefix:  Parallel procs.:

          []  [RPush; (RPop None); (RIs_empty true); (RSize 0)]

              [(RSteal (Some 73)); (RSize -1); (RSize 0)]

================================================================================
failure (1 tests failed, 0 tests errored, ran 1 tests)
```


Segfault in Domainslib (known, fixed)
-------------------------------------

Testing `Domainslib.Task`s with one dependency and with 2 work pools
found a [segfault in domainslib](https://github.com/ocaml-multicore/domainslib/issues/58).
As of [domainslib#100](https://github.com/ocaml-multicore/domainslib/pull/100)
the `domainslib/task_one_dep.ml` test in question has been moved to
the `Domainslib` repo.




Dead-lock in Domainslib (known, fixed)
--------------------------------------

A reported deadlock in domainslib motivated the development of these tests:
 - https://github.com/ocaml-multicore/domainslib/issues/47
 - https://github.com/ocaml-multicore/ocaml-multicore/issues/670

The tests `domainslib/task_one_dep.ml` and
`domainslib/task_more_deps.ml` were run with a timeout to prevent
deadlocking indefinitely. `domainslib/task_one_dep.ml` could trigger one
such deadlock. As of [domainslib#100](https://github.com/ocaml-multicore/domainslib/pull/100)
these tests have been moved to the `Domainslib` repo.


The test exhibits no non-determistic behaviour when repeating the same
tested property from within the QCheck test.
However it fails (due to timeout) on the following test input:

```ocaml
$ dune exec -- src/task_one_dep.exe -v
random seed: 147821373
generated error fail pass / total     time test name
[✗]   25    0    1   24 /  100    36.2s Task.async/await

--- Failure --------------------------------------------------------------------

Test Task.async/await failed (2 shrink steps):

{ num_domains = 3; length = 6;
  dependencies = [|None; (Some 0); None; (Some 1); None; None|] }
================================================================================
failure (1 tests failed, 0 tests errored, ran 1 tests)
```

This corresponds to the following program with 3+1 domains and 6 promises.
It loops infinitely with both bytecode/native:

```ocaml
...
open Domainslib

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  for _ = 1 to 200 do
    assert (7 = tak 18 12 6);
  done

let pool = Task.setup_pool ~num_additional_domains:3 ()

let p0 = Task.async pool work
let p1 = Task.async pool (fun () -> work (); Task.await pool p0)
let p2 = Task.async pool work
let p3 = Task.async pool (fun () -> work (); Task.await pool p1)
let p4 = Task.async pool work
let p5 = Task.async pool work

let () = List.iter (fun p -> Task.await pool p) [p0;p1;p2;p3;p4;p5]
let () = Task.teardown_pool pool
```


Utop segfault (known?, status?)
-------------------------------

Utop segfaults when loading [src/domain/domain_spawntree.ml](src/domain/domain_spawntree.ml)
interactively:

``` ocaml
$ utop
──────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────
                                              │ Welcome to utop version 2.8.0 (using OCaml version 4.12.0+domains)! │
                                              └─────────────────────────────────────────────────────────────────────┘
Findlib has been successfully loaded. Additional directives:
  #require "package";;      to load a package
  #list;;                   to list the available packages
  #camlp4o;;                to load camlp4 (standard syntax)
  #camlp4r;;                to load camlp4 (revised syntax)
  #predicates "p,q,...";;   to set these predicates
  Topfind.reset();;         to force that packages will be reloaded
  #thread;;                 to enable threads


Type #utop_help for help about using utop.

utop # #require "ppx_deriving.show";;
utop # #require "qcheck";;
utop # #use "src/domain_spawntree.ml";;
type cmd = Incr | Decr | Spawn of cmd list
val pp_cmd : Format.formatter -> cmd -> unit = <fun>
val show_cmd : cmd -> string = <fun>
val count_spawns : cmd -> int = <fun>
val gen : int -> int -> cmd Gen.t = <fun>
val shrink_cmd : cmd Shrink.t = <fun>
val interp : int -> cmd -> int = <fun>
val dom_interp : int Atomic.t -> cmd -> unit = <fun>
val t : max_depth:int -> max_width:int -> Test.t = <fun>
random seed: 359528592
Segmentation fault (core dumped)
```

This does not happen when running a plain `ocaml` top-level though, so it
seems `utop`-specific.
