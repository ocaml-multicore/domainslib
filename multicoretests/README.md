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
  let init () = IntBtree.init ()
  let cleanup _ = ()
  let run pool state ops = let ops = Array.of_list ops in IntBtree.run state pool ops
    
  let api = [
    val_ "insert" IntBtree.Sequential.insert (fun set _ key vl -> IntBtree.(Mk (Insert (key,vl), set))) 
                  (t @-> int @-> int @-> returning unit);
    val_ "search" IntBtree.Sequential.search (fun set _ key -> IntBtree.(Mk (Search key, set)))
                  (t @-> int @-> returning (option int));
    val_ "size" IntBtree.Sequential.size (fun set _ -> IntBtree.(Mk (Size, set)))
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
bindings `init` and `cleanup` for setting it up and tearing it down,
and `run` for running batches of operations.
The `api` then contains a list of type signature descriptions using
combinators `unit`, `bool`, `int`, `returning`, `returning_or_exc`,
... in the style of [Ctypes](https://github.com/ocamllabs/ocaml-ctypes).
The functor `Lin_batched.Make` expects a description of the tested
commands and outputs a module with a QCheck test `lin_test` that
performs the linearization test.

The QCheck linearization test iterates a number of test
instances. Each instance consists of a "sequential prefix" of calls to
the above commands, followed by a call to `run` with a batched
sequence of operations. `Lin_batched` chooses the individual
operations and arguments arbitrarily and records their results. The
framework then performs a search for a sequential permutation of the
same calls with the same results, and succeeds if it finds one.

