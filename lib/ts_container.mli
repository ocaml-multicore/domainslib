type 'a t
(** Type of Thread-Safe Container *)

val create : ?batch_limit:int -> unit -> 'a t
(** [create ?batch_limit ()] creates an instance of a Thread-Safe Container with an upper limit `batch_limit` when `get` is called. *)

val add : 'a t -> 'a -> unit
(** [add t elt] adds an element to the container. It is possible to add to the container even if the number of elements in the container is equal or greater than the batch limit. *)

val get : 'a t -> 'a array * int
(** [get t] retrieves a tuple with an array of the size of the batch_limit and an integer representing the number of valid operations from index position 0 *)

val size : 'a t -> int