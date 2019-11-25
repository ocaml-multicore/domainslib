type 'a t
(** The type of bounded channels *)

val make : int -> 'a t
(** [make n] makes a bounded channel with a buffer of size [n]. Raises
 * [Invalid_argument "Chan.make"] if the buffer size is less than 0.
 *
 * With a buffer size of 0, the send operation becomes synchronous. With a
 * buffer size of 1, you get the familiar MVar structure. The channel may be
 * shared between many sending and receiving domains. *)

val send : 'a t -> 'a -> unit
(** [send c v] sends the values [v] over the channel [c]. If the channel buffer
 * is full then the sending domain blocks until space becomes available. *)

val recv : 'a t -> 'a
(** [recv c] returns a value [v] received over the channel. If the channel
 * buffer is empty then the domain blocks until a message is sent on the
 * channel. *)
