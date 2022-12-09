type !'a t
(** The type of channels *)

val make_bounded : int -> 'a t
(** [make_bounded n] makes a bounded channel with a buffer of size [n]. Raises
    [Invalid_argument "Chan.make_bounded"] if the buffer size is less than 0.

    With a buffer size of 0, the send operation becomes synchronous. With a
    buffer size of 1, you get the familiar MVar structure. The channel may be
    shared between many sending and receiving domains. *)

val make_unbounded : unit -> 'a t
(** Returns an unbounded channel *)

val send : 'a t -> 'a -> unit
(** [send c v] sends the values [v] over the channel [c]. If the channel buffer
    is full then the sending domain blocks until space becomes available. *)

val send_poll : 'a t -> 'a -> bool
(** [send_poll c v] attempts to send the value [v] over the channel [c]. If the
    channel buffer is not full, the message is sent and returns [true]. Otherwise,
    returns [false]. *)

val recv : 'a t -> 'a
(** [recv c] returns a value [v] received over the channel. If the channel
    buffer is empty then the domain blocks until a message is sent on the
    channel. *)

val recv_poll : 'a t -> 'a option
(** [recv_poll c] attempts to receive a message on the channel [c]. If a
    message [v] is available on the channel then [Some v] is returned.
    Otherwise, returns [None]. *)
