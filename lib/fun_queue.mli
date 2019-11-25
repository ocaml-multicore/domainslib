type 'a t
(** The type of functional queue *)

val empty : 'a t
(** Empty queue *)

val length : 'a t -> int
(** Returns the length of the queue *)

val push : 'a t -> 'a -> 'a t
(** [push q v] returns a new queue with [v] pushed to the back of [q] *)

val pop : 'a t -> ('a * 'a t) option
(** [pop q] returns [None] if the queue is empty. If the queue is non-empty, it
 * returns [Some (v,q')] where [v] is the element popped from the head of [q]
 * and [q'] is the rest of the queue. *)
