(** This module allows the user to describe the type signature of
    a tested module interface using a DSL of type combinators.
*)

(** Internal module to build test representations.
    This module is exposed for internal uses only, its API may change
    at any time.
 *)
module Internal : sig
  module type CmdSpec = sig
    type t
    (** The type of the system under test *)

    type cmd
    (** The type of commands *)

    val show_cmd : cmd -> string
    (** [show_cmd c] returns a string representing the command [c]. *)

    val gen_cmd : cmd QCheck.Gen.t
    (** A command generator. *)

    val shrink_cmd : cmd QCheck.Shrink.t
    (** A command shrinker.
        To a first approximation you can use {!QCheck.Shrink.nil}. *)

    type res
    (** The command result type *)

    val show_res : res -> string
    (** [show_res r] returns a string representing the result [r]. *)

    val equal_res : res -> res -> bool
    (** equality function over {!res} *)

    val init : unit -> t
    (** Initialize the system under test. *)

    val cleanup : t -> unit
    (** Utility function to clean up [t] after each test instance,
        e.g., for closing sockets, files, or resetting global parameters *)

    val run : cmd -> t -> res
    (** [run c t] should interpret the command [c] over the system under test [t] (typically side-effecting). *)
  end

  module Make(Spec : CmdSpec) : sig
    val arb_cmds_triple : int -> int -> (Spec.cmd list * Spec.cmd list * Spec.cmd list) QCheck.arbitrary
    val check_seq_cons : (Spec.cmd * Spec.res) list -> (Spec.cmd * Spec.res) list -> (Spec.cmd * Spec.res) list -> Spec.t -> Spec.cmd list -> bool
    val interp_plain : Spec.t -> Spec.cmd list -> (Spec.cmd * Spec.res) list
    val lin_test : rep_count:int -> retries:int -> count:int -> name:string -> lin_prop:(Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool) -> QCheck.Test.t
    val neg_lin_test : rep_count:int -> retries:int -> count:int -> name:string -> lin_prop:(Spec.cmd list * Spec.cmd list * Spec.cmd list -> bool) -> QCheck.Test.t
  end

  val pp_exn : Format.formatter -> exn -> unit
  (** Format-based exception pretty printer *)
end
  [@@alert internal "This module is exposed for internal uses only, its API may change at any time"]


(** {1 Type-representing values} *)

type constructible = |
(** Type definition to denote whether a described type can be generated *)

type deconstructible = |
(** Type definition to denote whether a described type can be deconstructed,
    i.e., tested for equality. *)

type combinable
(** Type definition to denote that a described type can be composed with
    other combinators such as {!list}. *)

type noncombinable
(** Type definition to denote that a described type cannot be composed with
    other combinators such as {!list}. *)

type (_, _, _, _) ty
(** Type definition for type-describing combinators.
    [(typ,con,styp,comb) ty] represents a type [typ] and with an underlying state of type [styp].
    The [con] type parameter indicates whether the combinator type is {!constructible} or {!deconstructible}.
    The [comb] type parameter indicates whether the combinator type is {!combinable} or {!noncombinable}.
*)

val gen : 'a QCheck.arbitrary -> ('a -> string) -> ('a, constructible, 's, combinable) ty
(** [gen arb to_str] builds a {!constructible} and {!combinable} type combinator
    from a QCheck generator [arb] and a to-string function [to_str]. *)

val deconstructible : ('a -> string) -> ('a -> 'a -> bool) -> ('a, deconstructible, 's, combinable) ty
(** [deconstructible to_str eq] builds a {!deconstructible} and {!combinable} type combinator
    from a to-string function [to_str] and an equality predicate [eq]. *)

val gen_deconstructible : 'a QCheck.arbitrary -> ('a -> string) -> ('a -> 'a -> bool) -> ('a, 'c, 's, combinable) ty
(** [gen_deconstructible arb to_str eq] builds a {!combinable} type combinator
    from a QCheck generator [arb], a to-string function [to_str] and an
    equality predicate [eq]. *)


(** {2 Type combinators} *)

val unit : (unit, 'a, 'b, combinable) ty
(** The [unit] combinator represents the {{!Stdlib.Unit.t}[unit]} type *)

val bool : (bool, 'a, 'b, combinable) ty
(** The [bool] combinator represents the {{!Stdlib.Bool.t}[bool]} type *)

val char : (char, 'a, 'b, combinable) ty
(** The [char] combinator represents the {{!Stdlib.Char.t}[char]} type.
    It uses a uniform generator based on {!QCheck.char}. *)

val char_printable : (char, 'a, 'b, combinable) ty
(** The [char_printable] combinator represents the {{!Stdlib.Char.t}[char]} type.
    The generated characters have character codes 32-126 or 10 (newline)
    and are based on {!QCheck.printable_char}. *)

val nat_small : (int, 'a, 'b, combinable) ty
(** The [nat_small] combinator represents the {{!Stdlib.Int.t}[int]} type.
    The generated integers are non-negative, less than 100,
    and are based on {!QCheck.small_nat}. *)

val int : (int, 'a, 'b, combinable) ty
(** The [int] combinator represents the {{!Stdlib.Int.t}[int]} type.
    It uses a uniform generator based on {!QCheck.int}. *)

val int_small : (int, 'a, 'b, combinable) ty
(** The [int_small] combinator represents the {{!Stdlib.Int.t}[int]} type.
    The generated integers are non-negative
    and are based on {!QCheck.small_int}. *)

val int_pos : (int, 'a, 'b, combinable) ty
(** The [int_pos] combinator represents the {{!Stdlib.Int.t}[int]} type.
    The generated integers are non-negative
    and uniformly distributed. It is based on {!QCheck.pos_int}. *)

val int_bound : int -> (int, 'a, 'b, combinable) ty
(** The [int_bound b] combinator represents the {{!Stdlib.Int.t}[int]} type.
    The generated integers range from [0] to [b], inclusive.
    It uses a uniform generator based on {!QCheck.int_bound}.

    Note: the result of [int_bound my_bound] cannot be used both as an
    argument type and as a result type in type signature descriptions. *)

val int32 : (Int32.t, 'a, 'b, combinable) ty
(** The [int32] combinator represents the {{!Stdlib.Int32.t}[int32]} type.
    It uses a uniform generator based on {!QCheck.int32}. *)

val int64 : (Int64.t, 'a, 'b, combinable) ty
(** The [int64] combinator represents the {{!Stdlib.Int64.t}[int64]} type.
    It uses a uniform generator based on {!QCheck.int64}. *)

val nat64_small : (Int64.t, 'a, 'b, combinable) ty
(** The [nat64_small] combinator represents the {{!Stdlib.Int64.t}[int64]} type.
    The generated integers are non-negative
    and are based on {!QCheck.small_nat}. *)

val float : (float, 'a, 'b, combinable) ty
(** The [float] combinator represents the {{!Stdlib.Float.t}[float]} type.
    The generated floating point numbers do not include nan and infinities.
    It is based on {!QCheck.float}. *)

val string : (String.t, 'a, 'b, combinable) ty
(** The [string] combinator represents the {{!Stdlib.String.t}[string]} type.
    The generated strings have a size generated from {!QCheck.Gen.nat} and
    characters resulting from {!QCheck.char}. It is based on {!QCheck.string}. *)

val string_small : (String.t, 'a, 'b, combinable) ty
(** The [small_string] combinator represents the {{!Stdlib.String.t}[string]} type.
    The generated strings have a size generated from {!QCheck.Gen.small_nat} and
    characters resulting from {!QCheck.char}. It is based on {!QCheck.small_string}. *)

val string_small_printable : (String.t, 'a, 'b, combinable) ty
(** The [small_string] combinator represents the {{!Stdlib.String.t}[string]} type.
    The generated strings have a size generated from {!QCheck.Gen.small_nat} and
    characters resulting from {!QCheck.printable_char}.
    It is based on {!QCheck.small_printable_string}. *)

val option :
  ?ratio:float ->
  ('a, 'c, 's, combinable) ty -> ('a option, 'c, 's, combinable) ty
(** The [option] combinator represents the {{!Stdlib.Option.t}[option]} type.
    The generated values from [option t] are either [Some v] or [None]
    with [v] being generated by the [t] combinator.
    An optional [ratio] allows to change the default [0.85] [Some]s.
    It is based on {!QCheck.option}. *)

val opt :
  ?ratio:float ->
  ('a, 'b, 'c, combinable) ty -> ('a option, 'b, 'c, combinable) ty
(** The [opt] combinator is an alias for {!option}. *)

val list : ('a, 'c, 's, combinable) ty -> ('a list, 'c, 's, combinable) ty
(** The [list] combinator represents the {{!Stdlib.List.t}[list]} type.
    The generated lists from [list t] have a length resulting from {!QCheck.Gen.nat}
    and have their elements generated by the [t] combinator.
    It is based on {!QCheck.list}. *)

val array : ('a, 'c, 's, combinable) ty -> ('a array, 'c, 's, combinable) ty
(** The [array] combinator represents the {{!Stdlib.Array.t}[array]} type.
    The generated arrays from [array t] have a length resulting from {!QCheck.Gen.nat}
    and have their elements generated by the [t] combinator.
    It is based on {!QCheck.array}. *)

val seq : ('a, 'c, 's, combinable) ty -> ('a Seq.t, 'c, 's, combinable) ty
(** The [seq] combinator represents the {!Stdlib.Seq.t} type.
    The generated sequences from [seq t] have a length resulting from {!QCheck.Gen.nat}
    and have their elements generated by the [t] combinator. *)

val t : ('a, constructible, 'a, noncombinable) ty
(** The [t] combinator represents the type {!Spec.t} of the system under test. *)

val state : ('a, constructible, 'a, noncombinable) ty
(** The [state] combinator represents the type {!Spec.t} of the system under test.
    It is an alias for the [t] combinator. *)

val or_exn :
  ('a, deconstructible, 'b, combinable) ty ->
  (('a, exn) result, deconstructible, 'c, combinable) ty
(** The [or_exn] combinator transforms a result type representing [t]
  into a [(t, exn)] {{!Stdlib.Result.t}[result]} type. *)

val print_result :
  ('a -> string) -> ('b -> string) -> ('a, 'b) result -> string
(** [print_result pa pb] creates a to-string function for a [(a,b)] {{!Stdlib.Result.t}[result]} type
    given two to-string functions for [a]s and [b]s, respectively. *)

val print : ('a, 'c, 's, 'comb) ty -> 'a -> string
(** Given a description of type ['a], print a value of type ['a]. *)

val equal : ('a, deconstructible, 's, 'comb) ty -> 'a -> 'a -> bool
(** Given a description of type ['a], compare two values of type ['a]. *)


(** {1 Values representing API functions} *)

module Fun : sig
  (** [(ftyp,rtyp,styp) Fun.fn] represents a function type of type [ftyp], with return type [rtyp],
      and with the underlying state type [styp]. *)
  type (_, _, _) fn
end

val returning :
  ('a, deconstructible, 'b, combinable) ty -> ('a, 'a, 'b) Fun.fn
(** [returning t] represents a pure return type. *)

val returning_or_exc :
  ('a, deconstructible, 'b, combinable) ty ->
  ('a, ('a, exn) result, 'b) Fun.fn
(** [returning_or_exc t] represents a return type of a function that may raise an exception. *)

val returning_ : ('a, 'b, 'c, 'd) ty -> ('a, unit, 'c) Fun.fn
(** [returning_ t] represents a return type that should be ignored. *)

val returning_or_exc_ :
  ('a, 'b, 'c, 'd) ty -> ('a, (unit, exn) result, 'c) Fun.fn
(** [returning_or_exc_ t] represents a return type that should be ignored of a function
    that may raise an exception. *)

val ( @-> ) :
  ('a, constructible, 'b, 'c) ty ->
  ('d, 'e, 'b) Fun.fn -> ('a -> 'd, 'e, 'b) Fun.fn
(** [at @-> rt] represents a function type expecting an argument [at]
    and returning [rt]. *)


(** {1 API description} *)

(** Type and constructor to capture a single function signature *)
type !_ elem

type 's api = (int * 's elem) list
(** The type of module signatures *)

val val_ : string -> 'f -> ('f, 'r, 's) Fun.fn -> int * 's elem
(** [val_ str f sig] describes a function signature from a string [str],
    a function value [f], and a signature description [sig]. *)

val val_freq : int -> string -> 'f -> ('f, 'r, 's) Fun.fn -> int * 's elem
(** [val_freq w str f sig] describes a function signature like
    {!val_} [str f sig] but with relative weight [w] rather than 1.
    A function of weight 2 will have twice the probability of being
    invoked compared to a function of weight 1. *)


(** The required description of a module signature *)
module type Spec =
sig
  type t
  (** The type of the system under test *)

  val init : unit -> t
  (** The function to initialize the system under test *)

  val cleanup : t -> unit
  (** The function to cleanup the system under test *)

  val api : (int * t elem) list
  (** A description of the function signatures *)
end


(** {1 Generating a linearization testing module from an API} *)

module MakeCmd (_ : Spec) : Internal.CmdSpec [@alert "-internal"]
(** Functor to map a combinator-based module signature description
    into a raw {!Lin} description.
    This functor is exposed for internal uses only, its API may change
    at any time.
    *)
