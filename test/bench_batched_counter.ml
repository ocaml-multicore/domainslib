module T = Domainslib.Task

let nb = 1_000_000
let domains = Domain.recommended_domain_count () - 1

module Bench (C : Counters.S) = struct

  let test ~pool num_domains () =
    let t = C.create nb in
    let chunk_size = nb / (num_domains * 8) in 
    T.run pool (fun () ->
        T.parallel_for pool ~chunk_size ~start:1 ~finish:nb ~body:(fun _ -> C.increment pool t)
      );
    assert (C.unsafe_get t = nb)

  let run num_domains =
    let pool = T.setup_pool ~num_domains () in
    T.run pool (test ~pool num_domains) ;
    T.teardown_pool pool

  let run () =
    for i = 1 to domains do
      let t0 = Unix.gettimeofday () in
      run i ;
      let t1 = Unix.gettimeofday () in
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
  Format.printf "@." ;
  Format.printf "     num_domains: " ;
  for i = 1 to domains do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  Format.printf "     LockCounter: " ;
  Bench_LockCounter.run ();
  Format.printf " LockfreeCounter: " ;
  Bench_LockFreeCounter.run ();       
  Format.printf "  BatchedCounter: " ;
  Bench_BC_MPMC.run () ;
  Format.printf " BatchedCounterF: " ;
  Bench_BC_Fast.run () ;
  Format.printf "BatchedCounterFF: " ;
  Bench_BC_Faster.run ()

