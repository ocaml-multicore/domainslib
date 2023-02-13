(** Module for building parallel STM tests over {!Stdlib.Domain}s *)

module Make : functor (Spec : STM.Spec) ->
  sig
    val check_obs : (Spec.cmd * STM.res) list -> (Spec.cmd * STM.res) list -> (Spec.cmd * STM.res) list -> Spec.state -> bool
    (** [check_obs pref cs1 cs2 s] tests whether the observations from the sequential prefix [pref]
        and the parallel traces [cs1] [cs2] agree with the model started in state [s]. *)

    val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_cmds_triple seq_len par_len] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        All [cmds] are generated with {!Spec.arb_cmd}. *)

    val arb_triple : int -> int -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.state -> Spec.cmd QCheck.arbitrary) -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_triple seq_len par_len arb0 arb1 arb2] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [par_len] parallel commands each.
        The three {!Spec.cmd} components are generated with [arb0], [arb1], and [arb2], respectively.
        Each of these take the model state as a parameter. *)

    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * STM.res) list
    (** [interp_sut_res sut cs] interprets the commands [cs] over the system {!Spec.sut}
        and returns the list of corresponding {!Spec.cmd} and result pairs. *)

    val agree_prop_par : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Parallel agreement property based on {!Stdlib.Domain} *)

    val agree_test_par : count:int -> name:string -> QCheck.Test.t
    (** Parallel agreement test based on {!Stdlib.Domain} which combines [repeat] and [~retries] *)

    val neg_agree_test_par : count:int -> name:string -> QCheck.Test.t
    (** A negative agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

 end
