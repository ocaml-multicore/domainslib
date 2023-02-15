type 'a t
(** ['a t] is a finite vector storing elements of type ['a]. *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp f fmt vec] pretty prints the vector [vec] using [f] to print elements.  *)

val length: 'a t -> int
(** [length vec] returns the length of elements in [vec]. *)

val init : ?capacity:int -> unit -> 'a t
(** [init ?capacity ()] returns a fresh finite vector with maximum capacity [capacity]. *)

val init_with : ?capacity:int -> int -> (int -> 'a) -> 'a t
(** [init_with ?capacity n f] constructs a finite vector with maximum
    capacity [capacity] and [n] initial elements generated from
    [f].  *)

val singleton : ?capacity:int -> 'a -> 'a t
(** [singleton ?capacity elt] returns a finite vector with capacity [capacity] and initially 1 element [elt]. *)

val to_array : 'a t -> 'a array
(** [to_array vec] converts the finite array [vec] to a fresh array (updates to the array will not be reflected in the vector). *)

val get : 'a t -> int -> 'a
(** [get vec n] retrieves the [n]th element in [vec]. *)

val set : 'a t -> int -> 'a -> unit
(** [set vec n vl] sets the [n]th element in [vec] to [vl]. *)

val split_from : 'a t -> int -> 'a t
(** [split_from vec n] partitions [vec] into two halves, returning a
    new vector [vec] with the elements at index ([ind]...) onwards,
    and leaves the original vec with elements [ind - 1]. *)

val drop_last : 'a t -> unit
(** [drop_last] drops the last element in [vec]. *)

val insert : 'a t -> int -> 'a -> unit
(** [insert vec n vl] inserts element [vl] at index [n], shifting all subsequent elements up.  *)

val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_left f acc vec] folds [f] over the elements in [vec] with initial value [acc]. *)

val iter: ('a -> unit) -> 'a t -> unit
(** [iter f vec] iterates [f] over each element in [vec].  *)

val clip : 'a t -> int -> unit
(** [clip vec n] updates the length of [vec] to be [n], dropping all subsequent elements.  *)
