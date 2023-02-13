(** The Util module contains a number of reusable functions
    handy for multicore testing. *)


val repeat : int -> ('a -> bool) -> 'a -> bool
(** [repeat num prop] iterates a property [prop] [num] times. The function stops
    early and returns false if just one of the iterations returns false.
    This is handy if the property outcome is non-determistic, for example,
    if it depends on scheduling. *)

exception Timeout
(** exception raised by [prop_timeout] and [fork_prop_with_timeout]. *)

val prop_timeout : int -> ('a -> 'b) -> 'a -> 'b
(** [prop_timeout s prop] returns a property working as [prop] that times out and
    raises [Timeout] after [s] seconds. *)

val fork_prop_with_timeout : int -> ('a -> bool) -> 'a -> bool
(** [fork_prop_with_timeout s prop] tests a property in a separate process and
    times out and raises [Timeout] after [s] seconds, like [prop_timeout s prop].
    This is handy if the tested code can segfault or loop infinitely. *)

val print_vertical : ?fig_indent:int -> ('a -> string) -> 'a list -> string
(** [print_vertical pr cmds] returns a string representing a sequential trace.
    Optional [fig_indent] indicates how many spaces it should be indented (default: 3 spaces).
 *)

val print_triple_vertical :
  ?fig_indent:int ->
  ?res_width:int ->
  ?center_prefix:bool ->
  ('a -> string) -> 'a list * 'a list * 'a list -> string
(** [print_triple_vertical pr (xs,ys,zs)] returns a string representing a
    parallel trace, with [xs] printed first, and then [ys] and [zs] printed
    in parallel.
    Optional [fig_indent] indicates how many spaces it should be indented (default: 10 spaces).
    Optional [res_width] specifies the reserved width for printing each list entry (default: 20 chars).
    Optional [center_prefix] centers the sequential prefix if [true] (the default) and otherwise left-adjust it.
 *)

val protect : ('a -> 'b) -> 'a -> ('b, exn) result
(** [protect f] turns an [exception] throwing function into a [result] returning function. *)

val pp_exn : Format.formatter -> exn -> unit
(** Format-based exception pretty printer *)

val show_exn : (Format.formatter -> (Format.formatter -> exn -> unit) -> unit) -> string
(** Format-based exception to-string function *)

val equal_exn : exn -> exn -> bool
(** equality function for comparing exceptions *)
