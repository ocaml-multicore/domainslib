module T = Domainslib.Task

let nb = 1_000_000
let domains = Domain.recommended_domain_count () - 1

module Bench (C : Batched_counter.Counter) = struct

  let test ~pool () =
    let t = C.create nb in
    T.run pool (fun () -> 
        T.parallel_for pool ~start:1 ~finish:nb ~body:(fun _ -> C.increment pool t)
      );
    assert (C.unsafe_get t = nb)

  let run num_domains =
    let pool = T.setup_pool ~num_domains () in
    T.run pool (test ~pool) ;
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

module Bench_ParCounter = Bench (Batched_counter.ParCounter)
module Bench_LockCounter = Bench (Batched_counter.LockCounter)
module Bench_BC_MPMC = Bench (Batched_counter.BC_MPMC)
(* module Bench_BCArray = Bench (Batched_counter.BCArray) *)

let () = 
  Format.printf "@." ;
  Format.printf "num_domains: " ;
  for i = 1 to domains do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  (* Format.printf "  ParCounter: " ;
  Bench_ParCounter.run ();
  Format.printf " LockCounter: " ;
  Bench_LockCounter.run () ; *)
  Format.printf " BATCHED_MPMC: " ;
  Bench_BC_MPMC.run () ;