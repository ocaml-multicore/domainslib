module T = Domainslib.Task

let nb = 100_000
let add_domains = Domain.recommended_domain_count () - 1

module Bench (C : Counters.S) = struct

  let run () =
    for num_domains = 0 to add_domains do
      let pool = T.setup_pool ~num_domains () in
      let t0 = Unix.gettimeofday () in
      let t = C.create nb in
      T.run pool (fun () ->
          T.parallel_for pool ~chunk_size:(nb/4092) ~start:1 ~finish:nb ~body:(fun _ -> C.increment pool t)
        );
      assert (C.unsafe_get t = nb);
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
  Format.printf "@." ;
  Format.printf "     num_domains: " ;
  for i = 2 to add_domains + 1 do
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

