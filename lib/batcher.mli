module type BatchedDS =
  sig
    type 'a t = {
      pool : Task.pool;
      ds : 'a
    }
    (** Type of the Explicitly Batched Data Structure *)

    type 'a batch_op 
    (** GADT specifiying the metadata of the operation to be performed including the parameters of the operation exepected *)

    type wrapped_batch_op =
        Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op
    (** Wrapper to make 'a batch_op an existential type so that we can batch different operations together and pass them around easily *)

    val create : Task.pool -> 'a t
    (** [create pool] returns a new instance of the batched data structure. holding a reference to the pool to provide dynamic multi-threading functionality *)

    val bop : 'a t -> wrapped_batch_op array -> int -> unit
    (** [bop t op_array num] performs a batch-parallel operation on a array of `num` elements *)

    [@@@alert unsafe "For developer use"]
    val unload : 'a t -> 'a

    [@@@alert unsafe "For developer use"]
    val load : 'a t -> 'a -> unit

  end

module Make :
  functor (DS : BatchedDS) ->
    sig
      type 'a t
      type 'a batch_op = 'a DS.batch_op
      val create : Task.pool -> 'a t
      val batchify : 'a t -> 'a batch_op -> 'a
    end

module Make2 :
  functor (DS : BatchedDS) ->
    sig
      type 'a t
      type 'a batch_op = 'a DS.batch_op
      val create : Task.pool -> 'a t
      val unload : 'a t -> 'a
      [@@@alert unsafe "For developer use"]
      val load : 'a t -> 'a t -> unit
      [@@@alert unsafe "For developer use"]
      val batchify : 'a t -> 'a batch_op -> 'a
    end