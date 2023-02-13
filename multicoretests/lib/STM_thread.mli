(** Module for building concurrent STM tests over {!Thread}s *)

module Make : functor (Spec : STM.Spec) ->
  sig
    exception ThreadNotFinished

    val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    (** [arb_cmds_triple seq_len conc_len] generates a [cmd] triple with at most [seq_len]
        sequential commands and at most [conc_len] concurrent commands each.
        All [cmds] are generated with {!Spec.arb_cmd}. *)

    val interp_sut_res : Spec.sut -> Spec.cmd list -> (Spec.cmd * STM.res) list
    (** [interp_sut_res sut cs] interprets the commands [cs] over the system [sut]
        and returns the list of corresponding {!Spec.cmd} and result pairs. *)

    val agree_prop_conc : Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool
    (** Concurrent agreement property based on {!Thread} *)

    val agree_test_conc : count:int -> name:string -> QCheck.Test.t
    (** Concurrent agreement test based on {!Thread} which combines [repeat] and [~retries] *)

    val neg_agree_test_conc : count:int -> name:string -> QCheck.Test.t
    (** A negative agreement test (for convenience). Accepts two labeled parameters:
        [count] is the test count and [name] is the printed test name. *)

  end
  [@@alert experimental "This module is experimental: It may fail to trigger concurrency issues that are present."]
