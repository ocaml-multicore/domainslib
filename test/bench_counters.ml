module T = Domainslib.Task

let run_lock = true
let run_lockfree = true
let run_impbatch = true
let run_impbatchf = false
let run_impbatchff = false
let batch_size =  4096
let operations = 1_000_000
let add_domains = Domain.recommended_domain_count () - 1

module Bench (C : Counters.S) = struct
  let chunk_size = Util.chunk_calculator ~batch_size ~operations ()
  let run () =
    for num_domains = 0 to add_domains do
      let pool = T.setup_pool ~num_domains () in
      let t = C.create operations in
      let t0 = Unix.gettimeofday () in
      T.run pool (fun () ->
          T.parallel_for pool ~chunk_size ~start:1 ~finish:operations ~body:(fun _ -> C.increment pool t)
        );
      assert (C.unsafe_get t = operations);
      let t1 = Unix.gettimeofday () in
      T.teardown_pool pool;
      Format.printf "  %7s%!"
        (Printf.sprintf "%.2f" (1000.0 *. (t1 -. t0)))
    done ;
    Format.printf "@."
end

module Bench_BC_MPMC = Bench (Counters.BatchedCounter)
module Bench_BC_Fast = Bench (Counters.BatchedCounterFast)
module Bench_BC_Faster = Bench (Counters.BatchedCounterFaster)
module Bench_LockFreeCounter = Bench (Counters.LockfreeCounter)
module Bench_LockCounter = Bench (Counters.LockCounter)

let () =
  Printf.printf "\n\nRunning ImpBatch Counter Benchmarks, batch_size = %d, ops = %d\n%!" batch_size operations;
  Format.printf "@." ;
  Format.printf "     num_domains: " ;
  for i = 1 to add_domains + 1 do
    Format.printf " %5i   " i
  done ;
  if run_lock then (
    Format.printf "@." ;
    Format.printf "     LockCounter: " ;
    Bench_LockCounter.run ());
  if run_lockfree then (
    Format.printf " LockfreeCounter: " ;
    Bench_LockFreeCounter.run ());
  if run_impbatch then (
    Format.printf "  BatchedCounter: " ;
    Bench_BC_MPMC.run ());
  if run_impbatchf then (
    Format.printf " BatchedCounterF: " ;
    Bench_BC_Fast.run ());
  if run_impbatchff then (
    Format.printf "BatchedCounterFF: " ;
    Bench_BC_Faster.run ())

