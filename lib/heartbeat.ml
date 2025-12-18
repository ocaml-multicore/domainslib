(*
 * Heartbeat scheduling for Domainslib.Task
 *
 * Uses fiber-local state and runtime interrupt callbacks.
 * The fiber-local state stores a POINTER to the mutable token state,
 * so the heartbeat callback can update it directly without allocation.
 *
 * This module is internal - users should use Task.fork_join instead.
 *)

(* === C STUBS === *)

(* Setup the heartbeat with interval, dynamic key, and callback *)
external heartbeat_setup : int -> 'a -> ('a -> unit) -> unit = "domainslib_heartbeat_setup"
external heartbeat_acquire : unit -> unit = "domainslib_heartbeat_acquire"
external heartbeat_release : unit -> unit = "domainslib_heartbeat_release"

(* Create a dynamic variable *)
external create_dynamic : 'a -> 'a = "domainslib_create_dynamic"

(* Set fiber-local state directly on current fiber - fast! *)
external set_fiber_state : 'a -> 'b -> unit = "domainslib_set_fiber_state"

(* Get fiber-local state - walks fiber stack (unused but kept for API completeness) *)
external get_fiber_state : 'a -> 'b = "domainslib_get_fiber_state"
  [@@warning "-32"]

external heartbeat_stats : unit -> int * int = "domainslib_heartbeat_stats"
external heartbeat_reset_stats : unit -> unit = "domainslib_heartbeat_reset_stats"

(* === CONFIGURATION === *)

let heartbeat_interval_us = 500  (* 0.5ms *)
let tokens_per_heartbeat = 4
let initial_tokens = 16

(* === MUTABLE TOKEN STATE === *)

(* This is the actual mutable state. The fiber-local storage holds a POINTER to this. *)
type state = {
  mutable tokens : int;
}

(* Null state for when no heartbeat is active *)
let null_state = { tokens = 0 }

(* The dynamic key - stores a pointer to state. Called once at module load. *)
let dynamic_key = create_dynamic null_state

(* Heartbeat callback - invoked by the runtime when heartbeat fires.
   The runtime calls caml_dynamic_get which returns the state pointer.
   We then directly update the mutable tokens field - no allocation! *)
let heartbeat_callback (state : state) =
  if state != null_state then
    state.tokens <- state.tokens + tokens_per_heartbeat

(* === INITIALIZATION === *)

let initialized = Atomic.make false

let init () =
  if not (Atomic.exchange initialized true) then
    heartbeat_setup heartbeat_interval_us dynamic_key heartbeat_callback

(* === INTERNAL API (used by Task) === *)

(* Create a new state for this fiber *)
let make_state () : state =
  let state = { tokens = initial_tokens } in
  set_fiber_state dynamic_key state;
  state

(* Install an existing state on the current fiber (for spawned tasks) *)
let install_state (state : state) : unit =
  set_fiber_state dynamic_key state

(* Get the current fiber's state - uses fiber-local storage *)
external get_fiber_state_inline : 'a -> 'b = "domainslib_get_fiber_state"
let[@inline] current_state () : state =
  get_fiber_state_inline dynamic_key

(* Check if we have tokens to parallelize *)
let[@inline] has_tokens (state : state) = state.tokens > 0

(* Use a token for parallelization - takes state explicitly *)
let[@inline] use_token (state : state) =
  if state.tokens > 0 then begin
    state.tokens <- state.tokens - 1;
    true
  end else false

(* Use a token from the current fiber's state - no argument passing! *)
let[@inline] use_token_current () =
  let state = current_state () in
  if state.tokens > 0 then begin
    state.tokens <- state.tokens - 1;
    true
  end else false

(* Split tokens for a child task - does NOT install state *)
let[@inline] split_for_child (state : state) : state =
  let child_tokens = state.tokens / 2 in
  state.tokens <- state.tokens - child_tokens;
  { tokens = child_tokens }

(* Fork: create a child state with half the tokens *)
let[@inline] fork (state : state) : state =
  split_for_child state

(* Fork from current fiber's state - no argument needed! *)
let[@inline] fork_current () : state =
  split_for_child (current_state ())

(* === LIFECYCLE === *)

let start () =
  init ();
  heartbeat_acquire ()

let stop () =
  heartbeat_release ()

(* === DEBUG (internal) === *)

let stats = heartbeat_stats
let reset_stats = heartbeat_reset_stats
