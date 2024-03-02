type 'a t = 'a array

(** {1 Iterators} *)

val iter : ('a -> unit) -> 'a array -> Task.pool -> unit
(** [iter f a] applies function [f] in turn to all
   the elements of [a].  It is equivalent to
   [f a.(0); f a.(1); ...; f a.(length a - 1); ()]. *)

val iteri :  (int -> 'a -> unit) -> 'a array -> Task.pool -> unit
(** Same as {!iter}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val map :  ('a -> 'b) -> 'a array -> Task.pool -> 'b array
(** [map f a] applies function [f] to all the elements of [a],
   and builds an array with the results returned by [f]:
   [[| f a.(0); f a.(1); ...; f a.(length a - 1) |]]. *)

val map_inplace :  ('a -> 'a) -> 'a array -> Task.pool -> unit
(** [map_inplace f a] applies function [f] to all elements of [a],
    and updates their values in place.
    @since 5.1 *)

val mapi :  (int -> 'a -> 'b) -> 'a array -> Task.pool -> 'b array
(** Same as {!map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val mapi_inplace : (int -> 'a -> 'a) -> 'a array -> Task.pool -> unit
(** Same as {!map_inplace}, but the function is applied to the index of the
    element as first argument, and the element itself as second argument.
    @since 5.1 *)
