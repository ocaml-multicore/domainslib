module type BatchedDS = sig
  type t
  type 'a batch_op
  type wrapped_batch_op = 
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  val create : Task.pool -> t
  val bop : t -> wrapped_batch_op array -> int -> unit
end
module Make (DS : BatchedDS) = struct
  type 'a batch_op = 'a DS.batch_op
  type t = {
    pool : Task.pool;
    ds : DS.t;
    running : bool Atomic.t;
    container : DS.wrapped_batch_op Ts_container.t
  }

  (* Can we make this take variable arguments depending on the DS.create? *)
  let create pool = 
    { pool;
      ds = DS.create pool;
      running = Atomic.make false;
      container = Ts_container.create () }

  let rec try_launch t =
    if Ts_container.size t.container > 0 
    && Atomic.compare_and_set t.running false true 
    then
      begin
        let batch, size = Ts_container.get t.container in
        DS.bop t.ds batch size;
        Atomic.set t.running false;
        try_launch t
      end

  let batchify t op =
    let pr, set = Task.promise () in
    let op_set = DS.Batched_op (op, set) in
    Ts_container.add t.container op_set;
    try_launch t;
    Task.await t.pool pr

end
