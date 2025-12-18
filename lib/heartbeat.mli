(**
 * Internal heartbeat scheduling for Domainslib.Task
 *
 * This module is internal - users should use Task.fork_join and Task.maybe_fork.
 *)

(** {1 Internal State Type} *)

type state
(** Mutable fiber state for heartbeat scheduling. *)

(** {1 Internal API (used by Task)} *)

val make_state : unit -> state
(** Create a new fiber state with initial tokens on the current fiber. *)

val install_state : state -> unit
(** Install an existing state on the current fiber (for spawned tasks). *)

val has_tokens : state -> bool
(** Check if we have tokens available for parallelization. *)

val use_token : state -> bool
(** [use_token state] attempts to use a token.
    Returns [true] if available (spawn parallel); [false] otherwise. *)

val use_token_current : unit -> bool
(** [use_token_current ()] attempts to use a token from the current fiber's state.
    No argument passing needed - reads from fiber-local storage directly. *)

val split_for_child : state -> state
(** [split_for_child state] splits tokens between parent and child, returns child state. *)

val fork : state -> state
(** [fork state] creates a child state with half the tokens. Alias for split_for_child. *)

val fork_current : unit -> state
(** [fork_current ()] creates a child state from the current fiber's state.
    No argument passing needed - reads from fiber-local storage directly. *)

(** {1 Lifecycle (used by Task.setup_pool / teardown_pool)} *)

val start : unit -> unit
(** Start the heartbeat pthread and register the interrupt hook. *)

val stop : unit -> unit
(** Stop participating in heartbeat (decrement refcount). *)

(** {1 Debug/Testing} *)

val stats : unit -> int * int
(** [stats ()] returns [(heartbeat_count, callbacks_invoked)] for debugging. *)

val reset_stats : unit -> unit
(** Reset heartbeat statistics. *)

