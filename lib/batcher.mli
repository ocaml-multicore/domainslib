module type BatchedDS =
sig
  type t
  (** Type of the Explicitly Batched Data Structure *)

  type 'a batch_op 
  (** GADT specifiying the metadata of the operation to be performed including the parameters of the operation exepected *)

  type wrapped_batch_op =
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op
    (** Wrapper to make 'a batch_op an existential type so that we can batch different operations together and pass them around easily *)

  val create : unit -> t
  (** [create pool] returns a new instance of the batched data structure. holding a reference to the pool to provide dynamic multi-threading functionality *)

  val bop : t -> Task.pool -> wrapped_batch_op array -> int -> unit
  (** [bop t pool op_array num] accepts an `op_array` containing `num` arbritrary wrapped_batch_op and performs batch-parallel operations to completion *)

end
module Make :
  functor (DS : BatchedDS) ->
  sig
    type t
    type 'a batch_op = 'a DS.batch_op
    val create : Task.pool -> t
    val batchify : t -> 'a batch_op -> 'a

    val unload : t -> DS.t
    [@@@alert unsafe "For developer use"]
    val load : t -> DS.t -> unit
    [@@@alert unsafe "For developer use"]
  end

(* module type PolyBatchedDS =
sig
  type 'a t
  (** Type of the Explicitly Batched Data Structure *)

  type ('elt,_) batch_op 
  (** GADT specifiying the metadata of the operation to be performed including the parameters of the operation exepected *)

  type wrapped_batch_op =
      Batched_op : ('elt,_) batch_op * ('elt -> unit) -> wrapped_batch_op
    (** Wrapper to make 'a batch_op an existential type so that we can batch different operations together and pass them around easily *)

  val create : unit -> 'a t
  (** [create pool] returns a new instance of the batched data structure. holding a reference to the pool to provide dynamic multi-threading functionality *)

  val bop : 'a t -> Task.pool -> wrapped_batch_op array -> int -> unit
  (** [bop t pool op_array num] accepts an `op_array` containing `num` arbritrary wrapped_batch_op and performs batch-parallel operations to completion *)

end
module MakePoly :
  functor (DS : PolyBatchedDS) ->
  sig
    type 'a t
    type ('a,'b) batch_op = ('a, 'b) DS.batch_op
    val create : Task.pool -> 'a t
    val batchify : 'a t -> ('a, 'b) batch_op -> 'a

    val unload : 'a t -> 'a DS.t
    [@@@alert unsafe "For developer use"]
    val load : 'a t -> 'a DS.t -> unit
    [@@@alert unsafe "For developer use"]
  end *)