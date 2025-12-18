(* Test for Heartbeat module *)

module T = Domainslib.Task
module H = Domainslib.Heartbeat

let num_domains = 4

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

let rec fib_seq n =
  if n <= 1 then n
  else fib_seq (n-1) + fib_seq (n-2)

let () =
  Printf.printf "=== Heartbeat Module Test ===\n\n";

  let n = 35 in
  let expected = fib_seq n in
  Printf.printf "Sequential fib(%d) = %d\n" n expected;

  H.start ();
  let pool = T.setup_pool ~num_domains () in
  let result = T.run pool (fun () ->
    let state = H.make () in
    fib_heartbeat pool state n
  ) in
  T.teardown_pool pool;
  let (heartbeats, _) = H.stats () in
  H.stop ();

  Printf.printf "Heartbeat fib(%d) = %d (heartbeats: %d)\n" n result heartbeats;

  if result = expected then
    Printf.printf "\n=== PASSED ===\n"
  else begin
    Printf.printf "\n=== FAILED: expected %d, got %d ===\n" expected result;
    exit 1
  end
