open Lin

(** functor to build an internal module representing {!Stdlib.Effect}-based tests *)
module Make_internal (_ : Internal.CmdSpec [@alert "-internal"]) : sig
  module EffSpec : sig
    type cmd
  end
  val arb_cmds_triple : int -> int -> (EffSpec.cmd list * EffSpec.cmd list * EffSpec.cmd list) QCheck.arbitrary
  val lin_prop : (EffSpec.cmd list * EffSpec.cmd list * EffSpec.cmd list) -> bool
  val lin_test : count:int -> name:string -> QCheck.Test.t
  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
end
  [@@alert internal "This module is exposed for internal uses only, its API may change at any time"]

val fork : (unit -> unit) -> unit
(** Helper function to fork a process in the underlying {!Stdlib.Effect}-based scheduler
*)

val yield : unit -> unit
(** Helper function to yield control in the underlying {!Stdlib.Effect}-based scheduler
*)

(** functor to build a module for [Effect]-based testing *)
module Make (_ : Spec) : sig
  val lin_test : count:int -> name:string -> QCheck.Test.t
  (** [lin_test ~count:c ~name:n] builds an {!Stdlib.Effect}-based test with the name
      [n] that iterates [c] times. The test fails if one of the generated
      programs is not sequentially consistent. In that case it fails, and prints
      a reduced counter example.
  *)

  val neg_lin_test : count:int -> name:string -> QCheck.Test.t
  (** [neg_lin_test ~count:c ~name:n] builds a negative {!Stdlib.Effect}-based test with
      the name [n] that iterates [c] times. The test fails if no counter example
      is found, and succeeds if a counter example is indeed found, and prints it
      afterwards.
  *)
end
[@@alert experimental "This module is experimental: The interface is not considered stable, and it may fail to trigger concurrency issues that are present."]
