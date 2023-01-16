module type DS = sig
  type t
  type batch_op

  val batch_limit : int
  val create : Task.pool -> unit -> t
  val bop : t -> batch_op array -> int -> unit
end

module type TSContainer = sig
  type 'a t
  val create : batch_limit:int -> unit -> 'a t
  val add : 'a t -> 'a -> unit
  val get : 'a t -> 'a array * int
  val size : 'a t -> int
end 

module QCon : TSContainer = struct
  type 'a t = {
    chan : 'a Chan.t;
    size : int Atomic.t;
    batch_limit : int
  }
  let create ~batch_limit () = 
    {
      chan = Chan.make_unbounded ();
      size = Atomic.make 0;
      batch_limit
    }
  let add t elt =
    let _ = Atomic.fetch_and_add t.size 1 in
    Chan.send t.chan elt
  let get t = 
    let batch_size = Atomic.exchange t.size 0 in
    let limit = min batch_size t.batch_limit in
    let topup = max (batch_size - limit) 0 in
    let _ = Atomic.fetch_and_add t.size topup in
    Array.init limit (fun _ -> Chan.recv t.chan), batch_size
  let size t = Atomic.get t.size 
end

(* type container = ELT.t option array 
   type t = {
   switching : bool Atomic.t;
   primary : container Atomic.t;
   mutable secondary : container;
   size : int Atomic.t
   }

   let create ~batch_size () = {
   switching = Atomic.make false;
   primary = Atomic.make @@ Array.make batch_size None;
   secondary =  Array.make batch_size None;
   size = Atomic.make 0
   }

   let add (t : t) (elt : ELT.t) = 
   (* Make sure switching process is not happening *)
   while Atomic.get t.switching do () done;
   let slot = Atomic.fetch_and_add t.size 1 in
   t.container.(slot) <- Some elt

   let get t = 
   let size = Atomic.get t.size in
   let batch = Array.make t.size None in
   Array.blit t.container 0 batch 0 t.size;
   batch

   let size t = failwith "" *)

module Make (DS : DS) : sig
  type t
  type batch_op = DS.batch_op

  val create : Task.pool -> t
  val batch_limit : int
  val batchify : t -> batch_op -> 'a Task.promise -> 'a 
end = struct

  module Container = QCon

  type batch_op = DS.batch_op
  type t = {
    pool : Task.pool;
    ds : DS.t;
    running : bool Atomic.t;
    container : batch_op QCon.t
  }
  let batch_limit = DS.batch_limit
  let create pool = 
    { pool;
      ds = DS.create pool ();
      running = Atomic.make false;
      container = Container.create ~batch_limit () }

  let rec try_launch t =
    if Container.size t.container > 0 
    && Atomic.compare_and_set t.running false true 
    then
      begin
        let batch, size = Container.get t.container in
        DS.bop t.ds batch size;
        Atomic.set t.running false;
        try_launch t
      end

  let batchify t op_set pr =
    Container.add t.container op_set;
    try_launch t;
    Task.await t.pool pr

end
