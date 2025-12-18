(* Benchmark: fib without sequential cutoff

   This is the worst case for task creation overhead - every recursive call
   spawns a task. Heartbeat should help by deferring most task creations.
*)

module T = Domainslib.Task
module H = Domainslib.Heartbeat

let num_domains = 4
let n = 40  (* fib(40) = 102334155, larger problem for reliable timing *)

(* Fib with Heartbeat - no sequential cutoff *)
let rec fib_heartbeat pool state n =
  if n <= 1 then n
  else if H.use_token state then begin
    H.maybe_refill state;
    let child_state = H.split state in
    let p = T.async pool (fun () -> fib_heartbeat pool child_state (n - 1)) in
    let b = fib_heartbeat pool state (n - 2) in
    T.await pool p + b
  end else
    fib_heartbeat pool state (n - 1) + fib_heartbeat pool state (n - 2)

(* Fib that just creates closures but doesn't use fork_join2 *)
let rec fib_closures n =
  if n <= 1 then n
  else begin
    let f = (fun () -> fib_closures (n - 1)) in
    let g = (fun () -> fib_closures (n - 2)) in
    f () + g ()
  end

(* Fib with Task + manual sequential cutoff for comparison *)
let rec fib_task_cutoff pool n =
  if n <= 20 then  (* Sequential cutoff at 20 *)
    let rec seq_fib n = if n <= 1 then n else seq_fib (n-1) + seq_fib (n-2) in
    seq_fib n
  else
    let p = T.async pool (fun () -> fib_task_cutoff pool (n - 1)) in
    let b = fib_task_cutoff pool (n - 2) in
    let a = T.await pool p in
    a + b

(* Pure sequential fib for baseline *)
let rec fib_seq n =
  if n <= 1 then n
  else fib_seq (n-1) + fib_seq (n-2)

let time f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  (result, t1 -. t0)

let () =
  Printf.printf "=== Fib Benchmark (n=%d, %d domains) ===\n\n" n num_domains;

  (* Pure sequential baseline *)
  let result_seq, time_seq = time (fun () -> fib_seq n) in
  Printf.printf "Sequential:                    fib(%d) = %d, time = %.4fs\n" n result_seq time_seq;

  (* Closure allocation overhead *)
  let result_clos, time_clos = time (fun () -> fib_closures n) in
  Printf.printf "With closures (no fork):       fib(%d) = %d, time = %.4fs (%.1fx slower)\n"
    n result_clos time_clos (time_clos /. time_seq);

  (* Task with manual cutoff (baseline - how you should write it) *)
  let pool = T.setup_pool ~num_domains () in
  let result_cutoff, time_cutoff = time (fun () ->
    T.run pool (fun () -> fib_task_cutoff pool n)) in
  T.teardown_pool pool;
  Printf.printf "Task (with cutoff at 20):      fib(%d) = %d, time = %.4fs (%.1fx speedup)\n"
    n result_cutoff time_cutoff (time_seq /. time_cutoff);

  (* Task + Heartbeat without cutoff *)
  H.reset_stats ();
  H.start ();
  let pool = T.setup_pool ~num_domains () in
  let result_hb, time_hb = time (fun () ->
    T.run pool (fun () ->
      let state = H.make () in
      fib_heartbeat pool state n)) in
  let (heartbeats, tokens_used) = H.stats () in
  T.teardown_pool pool;
  H.stop ();
  Printf.printf "Heartbeat (no cutoff):         fib(%d) = %d, time = %.4fs (%.1fx speedup)\n"
    n result_hb time_hb (time_seq /. time_hb);
  Printf.printf "  Heartbeat stats: %d heartbeats, %d tokens used\n" heartbeats tokens_used;

  Printf.printf "\n";
  Printf.printf "Heartbeat vs optimal:     %.1fx slower (goal: ~1x)\n" (time_hb /. time_cutoff)
