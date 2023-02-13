(** Module with combinators and definitions to specify an STM test *)

(** Extensible type to represent result values *)
type 'a ty = ..

(** A range of constructors to represent built-in types *)
type _ ty +=
  | Unit : unit ty
  | Bool : bool ty
  | Char : char ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Float : float ty
  | String : string ty
  | Bytes : bytes ty
  | Exn : exn ty
  | Option : 'a ty -> 'a option ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty
  | List : 'a ty -> 'a list ty
  | Array : 'a ty -> 'a array ty
  | Seq : 'a ty -> 'a Seq.t ty

type 'a ty_show = 'a ty * ('a -> string)
(** Combinator type to represent an OCaml type along with an associated [to_string] function *)

val unit : unit ty_show
(** Combinator to represent the {{!Stdlib.Unit.t}[unit]} type *)

val bool : bool ty_show
(** Combinator to represent the {{!Stdlib.Bool.t}[bool]} type *)

val char : char ty_show
(** Combinator to represent the {{!Stdlib.Char.t}[char]} type *)

val int : int ty_show
(** Combinator to represent the {{!Stdlib.Int.t}[int]} type *)

val int32 : int32 ty_show
(** Combinator to represent the {{!Stdlib.Int32.t}[int32]} type *)

val int64 : int64 ty_show
(** Combinator to represent the {{!Stdlib.Int64.t}[int64]} type *)

val float : float ty_show
(** Combinator to represent the {{!Stdlib.Float.t}[float]} type *)

val string : string ty_show
(** Combinator to represent the {{!Stdlib.String.t}[string]} type *)

val bytes : bytes ty_show
(** Combinator to represent the {{!Stdlib.Bytes.t}[bytes]} type *)

val option : 'a ty_show -> 'a option ty_show
(** [option t] builds a [t] {{!Stdlib.Option.t}[option]} type representation *)

val exn : exn ty_show
(** Combinator to represent the [exception] type *)

val result : 'a ty_show -> 'b ty_show -> ('a,'b) Result.t ty_show
(** [result a b] builds an [(a,b)] {{!Stdlib.Result.t}[result]} type representation *)

val list : 'a ty_show -> 'a list ty_show
(** [list t] builds a [t] {{!Stdlib.List.t}[list]} type representation *)

val array : 'a ty_show -> 'a array ty_show
(** [array t] builds a [t] {{!Stdlib.Array.t}[array]} type representation *)

val seq : 'a ty_show -> 'a Seq.t ty_show
(** [seq t] builds a [t] {{!Stdlib.Seq.t}[Seq.t]} type representation *)

type res =
  Res : 'a ty_show * 'a -> res

val show_res : res -> string


(** The specification of a state machine. *)
module type Spec =
sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state *)

  type sut
  (** The type of the system under test *)

  val arb_cmd : state -> cmd QCheck.arbitrary
  (** A command generator. Accepts a state parameter to enable state-dependent {!cmd} generation. *)

  val init_state : state
  (** The model's initial state. *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  val next_state : cmd -> state -> state
  (** [next_state c s] expresses how interpreting the command [c] moves the
      model's internal state machine from the state [s] to the next state.
      Ideally a [next_state] function is pure. *)

  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the {!sut} after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c] in terms of the model state [s].
      A [precond] function should be pure.
      [precond] is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)

  val run : cmd -> sut -> res
  (** [run c i] should interpret the command [c] over the system under test (typically side-effecting). *)

  val postcond : cmd -> state -> res -> bool
  (** [postcond c s res] checks whether [res] arising from interpreting the
      command [c] over the system under test with {!run} agrees with the
      model's result. A [postcond] function should be a pure.

      {b Note:} the state parameter [s] is the model's {!state} before executing the command [c] (the "old/pre" state).
      This is helpful to model, e.g., a [remove] [cmd] that returns the removed element. *)
end


module Internal : sig
open QCheck
(** Internal helper module to build STM tests. *)


(** Derives a test framework from a state machine specification. *)
module Make (Spec : Spec) :
sig
  (** {3 The resulting test framework derived from a state machine specification} *)

    val cmds_ok : Spec.state -> Spec.cmd list -> bool
    (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
        Accepts the initial state and the command sequence as parameters.  *)

    val arb_cmds : Spec.state -> Spec.cmd list arbitrary
    (** A generator of command sequences. Accepts the initial state as parameter. *)

    val consistency_test : count:int -> name:string -> QCheck.Test.t
    (** A consistency test that generates a number of [cmd] sequences and
        checks that all contained [cmd]s satisfy the precondition [precond].
        Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

    val interp_agree : Spec.state -> Spec.sut -> Spec.cmd list -> bool
    (** Checks agreement between the model and the system under test
        (stops early, thanks to short-circuit Boolean evaluation). *)

    val check_disagree : Spec.state -> Spec.sut -> Spec.cmd list -> (Spec.cmd * res) list option
    (** [check_disagree state sut pg] checks that none of the commands present
        in [pg] violated the declared postconditions when [pg] is run in [state].
        Return [None] if none of the commands violate its postcondition, and
        [Some] list corresponding to the prefix of [pg] ending with the [cmd]
        violating its postcondition. *)

    val check_obs : (Spec.cmd * res) list -> (Spec.cmd * res) list -> (Spec.cmd * res) list -> Spec.state -> bool
    (** [check_obs pref cs1 cs2 s] tests whether the observations from the sequential prefix [pref]
        and the parallel traces [cs1] [cs2] agree with the model started in state [s]. *)

    val gen_cmds_size : (Spec.state -> Spec.cmd arbitrary) -> Spec.state -> int Gen.t -> Spec.cmd list Gen.t
    (** [gen_cmds_size arb state gen_int] generates a program of size generated
        by [gen_int] using [arb] to generate [cmd]s according to the current
        state. [state] is the starting state. *)

    val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    (** [arb_cmds_triple seq_len par_len] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each. *)

    val all_interleavings_ok : Spec.cmd list -> Spec.cmd list -> Spec.cmd list -> Spec.state -> bool
    (** [all_interleavings_ok seq spawn0 spawn1 state] checks that
        preconditions of all the {!cmd}s of [seq], [spawn0], and [spawn1] are satisfied in all the
        possible interleavings and starting with [state] *)

    val shrink_triple : (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) Shrink.t
    (** [shrink_triple arb0 arb1 arb2] is a {!QCheck.Shrink.t} for programs (triple of list of [cmd]s) that is specialized for each part of the program. *)

    val arb_triple : int -> int -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.state -> Spec.cmd arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) arbitrary
    (** [arb_triple seq_len par_len arb0 arb1 arb2] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        The three [cmd] components are generated with [arb0], [arb1], and [arb2], respectively.
        Each of these take the model state as a parameter. *)
end
  [@@alert internal "This module is exposed for internal uses only, its API may change at any time"]

end


val protect : ('a -> 'b) -> 'a -> ('b, exn) result
(** [protect f] turns an [exception]-throwing function into a {{!Stdlib.Result.t}[result]}-returning function. *)
