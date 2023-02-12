module type S = sig
  type t
  (** [t] represents a vanilla data structure. *)

  type 'a op 
  (** ['a op] represents a single operation on [t] with the return type ['a]. *)

  type wrapped_op = Mk : 'a op * ('a -> unit) -> wrapped_op
  (** [wrapped_op] represents an operation on the datastructure and
      the continuation to run after its completion.  *)

  val init : unit -> t
  (** [init ()] returns a new instance of the data structure. *)

  val run : t -> Task.pool -> wrapped_op array -> unit
  (** [run t pool ops num] when called with a data structure [t], and
      a thread pool [pool], executes all the operations in [ops],
      possibly using parallelism to improve the speed of the
      operation. *)

end

module Make : functor (S : S) -> sig

  type t
  (** [t] represents the type of a concurrent data structure *)

  type 'a op = 'a S.op
  (** ['a op] represents an operation  *)

  val init : Task.pool -> t
  (** [init pool] creates a new batched data structure, where [pool]
      will be used for parallelism. *)

  val apply : t -> 'a op -> 'a
  (** [apply t op] applies the operation [op] to [t]. *)

  val unsafe_get_internal_data : t -> S.t
  [@@@alert unsafe "For developer use"]

  val unsafe_set_internal_data : t -> S.t -> unit
  [@@@alert unsafe "For developer use"]

end
