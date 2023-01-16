# Domainslibs Fork - Batching support

This fork introduces support for a technique called "batching" which aims to increase data structure throughput of programs. In programs that run with "batching" support, data structure operations can be processed as a logical "batch" instead of atomic operations which reduces synchronization overhead and opens up more opportunities for optimisation. The goal of this work is to provide the scheduling infrastructure for the community to innovate and build new data structures for OCaml that stand to benefit from batching.

To read more about Batching, this paper introduces the idea https://www.cse.wustl.edu/~kunal/resources/Papers/batcher.pdf

# Preliminary work and test results
These benchmarks were run on an 8-core Intel x86-64 Machine
## Batched Counter
Simulating standard parallel counters against batched counters
```
(Workload: 1_000_000 increment operations)

                     (SEQ)
     num_domains:      1        2        3        4        5        6        7        8   
     batch limit:      ----------------------- 1_000_000 operations ------------------------
     LockCounter:    194.01   248.31   182.78   194.45   197.28   205.97   220.37   214.56  ms
 LockfreeCounter:    185.98   226.67   133.79   105.31    96.02   102.22    94.63    91.45  ms

  BatchedCounter:    368.80  1198.58   903.25   577.27   557.61   588.81   558.93   542.13  ms
 BatchedCounterF:    338.56  1204.10   855.68   718.11   664.72   621.39   570.63   555.96  ms
BatchedCounterFF:    189.30   177.86   154.45   137.15   175.45   209.81   211.35   244.32  ms
```
The batch_limit is configured by adjusing the chunk_size of `parallel_for` which determines the number of tasks grouped together to run asynchronously. A chunk_size of 1 corresponds to 1_000_000 operations spawned.

For standard parallel counters (LockCounter & LockfreeCounter), performance degrades as the number of cores increase. BatchedCounters on the other hand scale better, plateuing around 7 cores.

- BatchedCounter: Disjoint BOP operation, sequential chokepoint to create the "batch"
- BatchedCounterF: No sequential chokepoint from generating the "batch", integrated BOP
- BatchedCounterFF: Use fast and slow path

Turns out, there is a non-obvious "best" batch_limit.
```
`parallel_for` default batch_limit 

                      (SEQ)
     num_domains:       1        2        3        4        5        6        7        8 
     batch limit:       1        16       32       32       32       64       64       64
     LockCounter:     14.15   153.51    72.31    91.60   130.02   136.91   149.42   151.71  ms
 LockfreeCounter:      3.62    45.17    60.92    62.88    67.11    71.20    69.21    68.71  ms

  BatchedCounter:    123.09   517.09   409.30   383.17   323.59   321.24   302.19   337.08  ms
 BatchedCounterF:    102.82   492.39   396.41   355.16   289.69   277.99   265.32   260.26  ms
BatchedCounterFF:      3.65    35.73    14.61    19.94    19.44    21.80    24.64    24.21  ms


                     (SEQ)
     num_domains:      1        2        3        4        5        6        7        8
     batch limit:      ------------------------ 4096 operations ------------------------
     LockCounter:     16.56   136.70    74.34   109.33   129.83   140.08   134.13   152.98  ms
 LockfreeCounter:      5.36    45.93    67.13    72.48    80.48    85.65    90.23    90.95  ms

  BatchedCounter:    125.11   341.46   290.97   252.76   241.20   233.06   227.60   234.72  ms
 BatchedCounterF:    105.91   335.79   281.67   253.17   235.17   207.34   206.42   193.45  ms
BatchedCounterFF:      5.17    21.03    24.60    34.94    48.75    53.42    51.87    33.81  ms
```
My testing shows that a batch limit of 4096 operations per batch yields the best throughput for this batched counter. However, this "optimal" batch limit is highly dependent on performance of the batched operations as well as the workload of the core program.

### (SEQ) always performs better in both traditional synchronization and batching
1. Traditional synchronization - Overhead due to contention for shared resource dominates parallelism benefits
2. Batching - Overhead of batching dominates the speedup of batching

### (SEQ) performance difference between sections
The large difference in (SEQ) performance between the two sections are due to the number of tasks spawned. In the previous section, 1_000_000 tasks are spawned, causing contention in the scheduling. In the current section, the number of tasks spawned is greatly reduced, at most being 4096 tasks.

### Batching performance analysis
Like the earlier section, Batched counters from 2 -> 8 domains demonstrate mostly steady speedup and then some plateuing towards the end (Some slowdown is also observed). Batched counters are a unique case because the cost of increment or decrement operations are so cheap that the benefits of batching are hard to see. The below experiments are to show how `parallel_prefix_sums` which is the underlying batch operation without implicit batching shows marginal speedup after 5 domains. We also show how inserting delays to exaggerate the performance gain of batching reveals consistent speedup.

```
Performance of par_prefix_sums 10_000_000 ops (batch limit 4096)
    num_domains:      1        2        3        4        5        6        7        8
Par_prefix_sum:    375.19   144.77   124.04   104.43    90.87    83.14    84.19   109.89

Par_prefix_sum With sleep delay to exaggerate parallelism speedup (100_000 ops with 1ms delay)
   num_domains:      1        2        3        4        5        6        7        8
Par_prefix_sum:   5267.82  2634.60  1810.63  1316.54  1069.05   904.28   819.40   655.97

Implicit batching with sleep delay (100_000 ops with 1ms delay)
   num_domains:      1        2        3        4        5        6        7        8
BatchedCounter:   5268.99  2673.35  1844.65  1345.40  1096.41   929.03   830.48   694.64
```

Implicit Batch size statistics (batch limit 4096)
```
batch_size -> bop performed
1          -> 3
4          -> 1
5          -> 2
7          -> 1
12         -> 2
20         -> 1
29         -> 1
65         -> 1
101        -> 1
642        -> 1
1420       -> 1
2874       -> 1
3047       -> 1
3439       -> 1
3508       -> 1
3517       -> 2
3518       -> 3
3519       -> 74
3520       -> 34
3521       -> 1
3523       -> 2
3530       -> 1
3767       -> 1
4267       -> 1
4559       -> 1
4593       -> 1
4663       -> 1
4667       -> 2
4668       -> 1
4669       -> 4
4670       -> 11
4671       -> 61
4672       -> 39
```
## Batched Skip List
Skip-list sequential (No concurrency control) inserts vs batched inserts
```
Initialized: 1 Million elements
Inserts: 100,000 elements

       num_domains:      2        3        4        5        6        7        8
       batch limit:      16       32       32       32       64       64       64
           Seq_ins:     299      284      299      301      298      284      297  ops/ms
       Batched_ins:     346      432      465      563      585      644      627  ops/ms


       num_domains:      2        3        4        5        6        7        8
       batch limit:     ------------------------ 127 operations ------------------------
           Seq_ins:     299      284      299      301      298      284      297  ops/ms
       Batched_ins:     346      432      465      563      585      644      627  ops/ms

```
```
Initialized: 10 Million elements
Inserts: 100,000 elements

        num_domains:      2        3        4        5        6        7        8
        batch limit:     ------------------------ 127 operations ------------------------
        Batched_ins:     156      240      260      409      432      423      522  ops/ms
            Seq_ins:     137      142      153      153      149      153      149  ops/ms

```
BOP performance
```
Performance of parallel insert 1 million preset and 100_000 inserts (batch limit 127)
num_domains:      1        2        3        4        5        6        7        8
Batch_ins:       236      279      294      303      301      304      309      309

Performance of parallel insert (1 million preset, 1 million inserts)
num_domains:      1        2        3        4        5        6        7        8
Batch_ins:       459      730      845      886      896      909      914      924
```
Implicit Batch size statistics (batch limit 127)
```
batch_size -> bop performed
3          -> 2
18         -> 1
30         -> 5
31         -> 774
41         -> 1
50         -> 1
82         -> 1
94         -> 4
95         -> 776
```
## Notes
There is an interesting trade-off between the number of parallel operations running and the cost of parallelising tasks. It seems like creating huge batches, tests with batches as big as 15,000 does not beat tests with batches of size 60. This trade-off seems to be measured in the chunk_size calculation of the parallel-for algorithm. However, it does not always seem to be the best choice especially because it is dependent on how fast the batched operations run vs the sequential operations. I also suspect that if we can avoid the sequential bottle neck when we pass around the operation array, we may be able to attain more consistent behaviour

# Domainslib - Nested-parallel programming

Domainslib provides support for nested-parallel programming. Domainslib provides async/await mechanism for spawning parallel tasks and awaiting their results. On top of this mechanism, domainslib provides parallel iteration functions. At its core, domainslib has an efficient implementation of work-stealing queue in order to efficiently share tasks with other domains.

Here is a _sequential_ program that computes nth Fibonacci number using recursion:

```ocaml
(* fib.ml *)
let n = try int_of_string Sys.argv.(1) with _ -> 1

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let main () =
  let r = fib n in
  Printf.printf "fib(%d) = %d\n%!" n r

let _ = main ()
```

We can parallelise this program using Domainslib:

```ocaml
(* fib_par.ml *)
let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 1

(* Sequential Fibonacci *)
let rec fib n =
  if n < 2 then 1 else fib (n - 1) + fib (n - 2)

module T = Domainslib.Task

let rec fib_par pool n =
  if n > 20 then begin
    let a = T.async pool (fun _ -> fib_par pool (n-1)) in
    let b = T.async pool (fun _ -> fib_par pool (n-2)) in
    T.await pool a + T.await pool b
  end else
    (* Call sequential Fibonacci if the available work is small *)
    fib n

let main () =
  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let res = T.run pool (fun _ -> fib_par pool n) in
  T.teardown_pool pool;
  Printf.printf "fib(%d) = %d\n" n res

let _ = main ()
```

The parallel program scales nicely compared to the sequential version. The results presented below were obtained on a 2.3 GHz Quad-Core Intel Core i7 MacBook Pro with 4 cores and 8 hardware threads.

```bash
$ hyperfine './fib.exe 42' './fib_par.exe 2 42' \
            './fib_par.exe 4 42' './fib_par.exe 8 42'
Benchmark 1: ./fib.exe 42
  Time (mean ± sd):     1.217 s ±  0.018 s    [User: 1.203 s, System: 0.004 s]
  Range (min … max):    1.202 s …  1.261 s    10 runs

Benchmark 2: ./fib_par.exe 2 42
  Time (mean ± sd):    628.2 ms ±   2.9 ms    [User: 1243.1 ms, System: 4.9 ms]
  Range (min … max):   625.7 ms … 634.5 ms    10 runs

Benchmark 3: ./fib_par.exe 4 42
  Time (mean ± sd):    337.6 ms ±  23.4 ms    [User: 1321.8 ms, System: 8.4 ms]
  Range (min … max):   318.5 ms … 377.6 ms    10 runs

Benchmark 4: ./fib_par.exe 8 42
  Time (mean ± sd):    250.0 ms ±   9.4 ms    [User: 1877.1 ms, System: 12.6 ms]
  Range (min … max):   242.5 ms … 277.3 ms    11 runs

Summary
  './fib_par2.exe 8 42' ran
    1.35 ± 0.11 times faster than './fib_par.exe 4 42'
    2.51 ± 0.10 times faster than './fib_par.exe 2 42'
    4.87 ± 0.20 times faster than './fib.exe 42'
```

More example programs are available [here](https://github.com/ocaml-multicore/domainslib/tree/master/test).

## Installation

You can install this library using `OPAM`.

```bash
$ opam switch create 5.0.0+trunk --repo=default,alpha=git+https://github.com/kit-ty-kate/opam-alpha-repository.git
$ opam install domainslib
```

## Development

If you are interested in hacking on the implementation, then `opam pin` this repository:

```bash
$ opam switch create 5.0.0+trunk --repo=default,alpha=git+https://github.com/kit-ty-kate/opam-alpha-repository.git
$ git clone https://github.com/ocaml-multicore/domainslib
$ cd domainslib
$ opam pin add domainslib file://`pwd`
```
