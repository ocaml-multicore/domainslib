(*
 * Heartbeat scheduling for Domainslib.Task
 *
 * A background pthread periodically deposits tokens. At fork points, if tokens > 0,
 * spawn in parallel; otherwise run sequentially.
 *)

type t
(** Fiber state for heartbeat scheduling. *)

val make : unit -> t
(** Create a new fiber state with initial tokens. *)

val use_token : t -> bool
(** [use_token state] attempts to use a token. Returns [true] if available
    (spawn parallel); [false] otherwise (run sequential). *)

val maybe_refill : t -> unit
(** [maybe_refill state] periodically checks for new tokens from the heartbeat.
    Call this after [use_token] returns [true]. *)

val split : t -> t
(** [split state] creates a child state with half the parent's tokens. *)

val start : unit -> unit
(** Start the heartbeat pthread. *)

val stop : unit -> unit
(** Stop the heartbeat pthread. *)

val stats : unit -> int * int
(** [stats ()] returns [(heartbeat_count, tokens_used)] for debugging. *)

val reset_stats : unit -> unit
(** Reset heartbeat statistics. *)
