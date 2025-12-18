(* Benchmark: Heartbeat scheduling for parallel Fibonacci *)

module T = Domainslib.Task
module H = Domainslib.Heartbeat

let rec fib_seq n =
  if n <= 1 then n
  else fib_seq (n-1) + fib_seq (n-2)

let rec fib_cutoff pool n =
  if n <= 20 then fib_seq n
  else
    let p = T.async pool (fun () -> fib_cutoff pool (n - 1)) in
    let b = fib_cutoff pool (n - 2) in
    T.await pool p + b

let rec fib_heartbeat pool state n =
  if n <= 1 then n
  else if H.use_token state then begin
    H.maybe_refill state;
    let child = H.split state in
    let p = T.async pool (fun () -> fib_heartbeat pool child (n - 1)) in
    let b = fib_heartbeat pool state (n - 2) in
    T.await pool p + b
  end else
    fib_heartbeat pool state (n - 1) + fib_heartbeat pool state (n - 2)

let time f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. t0)

let () =
  let n = 42 in
  let num_domains = 4 in
  Printf.printf "=== Heartbeat Benchmark (n=%d, %d domains) ===\n\n" n num_domains;

  let result_seq, time_seq = time (fun () -> fib_seq n) in
  Printf.printf "Sequential:      fib(%d) = %d, time = %.3fs\n" n result_seq time_seq;

  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let result_cutoff, time_cutoff = time (fun () ->
    T.run pool (fun () -> fib_cutoff pool n)) in
  T.teardown_pool pool;
  Printf.printf "Cutoff (n<=20):  fib(%d) = %d, time = %.3fs (%.1fx)\n" 
    n result_cutoff time_cutoff (time_seq /. time_cutoff);

  H.start ();
  H.reset_stats ();
  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let result_hb, time_hb = time (fun () ->
    T.run pool (fun () -> fib_heartbeat pool (H.make ()) n)) in
  T.teardown_pool pool;
  let (beats, _) = H.stats () in
  H.stop ();
  Printf.printf "Heartbeat:       fib(%d) = %d, time = %.3fs (%.1fx)\n"
    n result_hb time_hb (time_seq /. time_hb);
  Printf.printf "  (%d heartbeats)\n" beats;

  Printf.printf "\nHeartbeat vs cutoff: %.2fx\n" (time_hb /. time_cutoff)
