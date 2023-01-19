module type BatchedDS =
  sig
    type t
    (** Type of the Batched Data Structure *)

    type 'a batch_op 
    (** GADT specifiying the metadata of the operation to be performed including the parameters of the operation exepected *)

    type wrapped_batch_op =
        Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op
    (** Wrapper to make 'a batch_op an existential type so that we can batch different operations together and pass them around easily *)

    val create : Task.pool -> t
    (** [create pool] returns a new instance of the batched data structure. holding a reference to the pool to provide dynamic multi-threading functionality *)

    val bop : t -> wrapped_batch_op array -> int -> unit
    (** [bop t op_array num] performs a batch-parallel operation on a array of `num` elements *)
  end

module Make :
  functor (DS : BatchedDS) ->
    sig
      type t
      type 'a batch_op = 'a DS.batch_op
      val create : Task.pool -> t
      val batchify : t -> 'a batch_op -> 'a
    end
