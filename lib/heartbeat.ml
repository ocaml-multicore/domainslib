(*
 * Heartbeat scheduling for Domainslib.Task
 *
 * A background pthread periodically deposits tokens. At fork points, if tokens > 0,
 * spawn in parallel; otherwise run sequentially.
 *)

(* === C STUBS === *)

external heartbeat_init : int -> int -> int -> unit = "domainslib_heartbeat_init"
external heartbeat_acquire : unit -> unit = "domainslib_heartbeat_acquire"
external heartbeat_release : unit -> unit = "domainslib_heartbeat_release"
external heartbeat_take_tokens : int -> int = "domainslib_heartbeat_take_tokens" [@@noalloc]
external heartbeat_stats : unit -> int * int = "domainslib_heartbeat_stats"
external heartbeat_reset_stats : unit -> unit = "domainslib_heartbeat_reset_stats"

(* === CONFIGURATION === *)

let max_domains = 128
let heartbeat_interval_us = 500
let tokens_per_heartbeat = 50
let check_interval = 200
let initial_tokens = 50

(* === INITIALIZATION === *)

let initialized = Atomic.make false

let init () =
  if not (Atomic.exchange initialized true) then
    heartbeat_init max_domains heartbeat_interval_us tokens_per_heartbeat

(* === FIBER STATE === *)

type t = {
  mutable local_tokens : int;
  mutable check_counter : int;
}

let make () = {
  local_tokens = initial_tokens;
  check_counter = check_interval;
}

(* === CORE API === *)

let[@inline] use_token state =
  if state.local_tokens > 0 then begin
    state.local_tokens <- state.local_tokens - 1;
    true
  end else false

let[@inline] maybe_refill state =
  state.check_counter <- state.check_counter - 1;
  if state.check_counter <= 0 then begin
    let slot = (Domain.self () :> int) in
    let from_global = heartbeat_take_tokens slot in
    if from_global > 0 then
      state.local_tokens <- state.local_tokens + from_global;
    state.check_counter <- check_interval
  end

let split state =
  let child_tokens = state.local_tokens / 2 in
  state.local_tokens <- state.local_tokens - child_tokens;
  { local_tokens = child_tokens; check_counter = check_interval }

(* === LIFECYCLE === *)

let start () =
  init ();
  heartbeat_acquire ()

let stop () =
  heartbeat_release ()

(* === DEBUG === *)

let stats = heartbeat_stats
let reset_stats = heartbeat_reset_stats
