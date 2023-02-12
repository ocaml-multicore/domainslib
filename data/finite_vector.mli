type 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val length: 'a t -> int

val init : ?capacity:int -> unit -> 'a t

val init_with : ?capacity:int -> int -> (int -> 'a) -> 'a t

val singleton : ?capacity:int -> 'a -> 'a t

val to_array : 'a t -> 'a array

val get : 'a t -> int -> 'a

val split_from : 'a t -> int -> 'a t

val drop_last : 'a t -> unit

val insert : 'a t -> int -> 'a -> unit

val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val iter: ('a -> unit) -> 'a t -> unit

val clip : 'a t -> int -> unit
