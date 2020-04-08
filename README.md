# domainslib - Domain-level Parallel Programming Library for Multicore OCaml

`domainslib` provides control and data structure for parallel programming using
domains.

The supported data structures are:

* [Channels](https://github.com/ocaml-multicore/domainslib/blob/master/lib/chan.mli)
  + Channels may be shared between multiple senders and receivers.
  + Channels come in two flavours -- bounded (fixed-buffer size (>= 0) beyond which the
    send blocks) and unbounded.
* [Task pools](https://github.com/ocaml-multicore/domainslib/blob/task_pool/lib/task_pool.mli)
  + Parallel for loop
  + fork-join parallelism

See
[examples](https://github.com/ocaml-multicore/domainslib/tree/task_pool/test)
for usage.
