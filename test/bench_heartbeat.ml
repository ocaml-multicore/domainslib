(* Benchmark: Compare manual cutoff vs heartbeat scheduling *)

module T = Domainslib.Task
module H = Domainslib.Heartbeat_internal

let num_domains = 8
let n = 42

(* Sequential fib *)
let rec fib n =
  if n < 2 then n
  else fib (n-1) + fib (n-2)

(* Manual cutoff at n <= 20 *)
let rec fib_cutoff pool n =
  if n <= 20 then fib n
  else
    let a = T.async pool (fun () -> fib_cutoff pool (n-1)) in
    let b = fib_cutoff pool (n-2) in
    T.await pool a + b

(* Heartbeat - simplest version, just pass hb state *)
let fib_heartbeat pool n =
  let module H = Domainslib.Heartbeat in
  let rec go hb n =
    if n < 2 then n
    else if H.use_token hb then begin
      let child_hb = H.fork hb in
      let a = T.async pool (fun () ->
        H.install_state child_hb;
        go child_hb (n-1)
      ) in
      let b = go hb (n-2) in
      T.await pool a + b
    end else
      go hb (n-1) + go hb (n-2)
  in
  go (H.make_state ()) n

let time_avg ~iters f =
  (* Warmup *)
  for _ = 1 to 2 do ignore (f ()) done;
  (* Timed runs *)
  let total = ref 0.0 in
  let result = ref 0 in
  for _ = 1 to iters do
    let t0 = Unix.gettimeofday () in
    result := f ();
    let t1 = Unix.gettimeofday () in
    total := !total +. (t1 -. t0)
  done;
  (!result, !total /. float_of_int iters)

let iters = 5

let () =
  Printf.printf "=== Fib(%d) Benchmark (domains=%d, iters=%d) ===\n\n" n num_domains iters;

  (* Sequential baseline *)
  let (result_seq, time_seq) = time_avg ~iters (fun () -> fib n) in
  Printf.printf "Sequential:       %.3fs (result=%d)\n" time_seq result_seq;

  (* Manual cutoff *)
  let pool = T.setup_pool ~num_domains () in
  let (result_cutoff, time_cutoff) = time_avg ~iters (fun () ->
    T.run pool (fun () -> fib_cutoff pool n)
  ) in
  T.teardown_pool pool;
  Printf.printf "Manual cutoff:    %.3fs (result=%d, speedup=%.2fx)\n"
    time_cutoff result_cutoff (time_seq /. time_cutoff);

  (* Heartbeat - passing only hb state, pool is captured *)
  let pool = T.setup_pool ~num_domains () in
  H.reset_stats ();
  let (result_hb, time_hb) = time_avg ~iters (fun () ->
    T.run pool (fun () ->
      fib_heartbeat pool n
    )
  ) in
  let (heartbeats, callbacks) = H.stats () in
  T.teardown_pool pool;
  Printf.printf "Heartbeat:        %.3fs (result=%d, speedup=%.2fx)\n"
    time_hb result_hb (time_seq /. time_hb);
  Printf.printf "  heartbeats=%d, callbacks=%d\n" heartbeats callbacks;

  Printf.printf "\nHeartbeat vs Cutoff: %.2fx\n" (time_hb /. time_cutoff);

  if result_seq = result_cutoff && result_seq = result_hb then
    Printf.printf "=== CORRECT ===\n"
  else
    Printf.printf "=== ERROR ===\n"
