(* module Atomic = struct *)
(*   include Atomic *)

(*   let n = 0.0001 *)
(*   let get t = Unix.sleepf n; Atomic.get t *)
(*   let incr t = Unix.sleepf n; Atomic.incr t *)
(*   let decr t = Unix.sleepf n; Atomic.decr t *)
(*   let fetch_and_add t = Unix.sleepf n; Atomic.fetch_and_add t *)
(*   let set t = Unix.sleepf n; Atomic.set t *)
(* end *)

module T = Domainslib.Task

module type CounterBaseS = sig
  module Q = Mpmc_queue
  type t =  {
    counter : int Atomic.t;
    running : bool Atomic.t;
    lock : Mutex.t;
    q : batch_op Q.t;
    container : batch_op array
  } and
  batch_op =
      | Incr of t * (unit -> unit)
      | Decr of t * (unit -> unit)
      | Get of t * (int -> unit)
      | Null
  val create : int -> t
  val unsafe_get : t -> int
end

module CounterBase : CounterBaseS = struct
  module Q = Mpmc_queue
  type t = {
    counter : int Atomic.t;
    running : bool Atomic.t;
    lock : Mutex.t;
    q : batch_op Q.t;
    container : batch_op array
  }
  and
    batch_op =
    | Incr of t * (unit -> unit)
    | Decr of t * (unit -> unit)
    | Get of t * (int -> unit)
    | Null

  let create n =
    {counter = Atomic.make 0; running = Atomic.make false; q = Q.make (); container = Array.make n Null; lock = Mutex.create ()}

  let unsafe_get t = Atomic.get t.counter
end

module type S = sig
  include CounterBaseS
  val increment : T.pool -> t -> unit
  val decrement : T.pool -> t -> unit
  val get : T.pool -> t -> int
end

module BatchedCounter : S = struct
  include CounterBase

  let par_prefix_sums pool t arr =
    let len = Array.length arr in
    let start = Atomic.get t.counter in
    let add_n = T.parallel_for_reduce pool ~start:0 ~finish:(len-1)
        ~body:(fun i ->
            match arr.(i) with
            | Incr (_, set) ->  set (); 1
            | Decr (_, set) ->  set (); -1
            | Get (_, set) -> set start; 0
            | Null -> failwith "Bad") ( + ) 0 in
    Atomic.set t.counter (start + add_n)

  let rec try_launch pool t =
    if Atomic.compare_and_set t.running false true then
      match Q.pop t.q with
      | Some op -> t.container.(0) <- op;
        (let i = ref 1 in
         while
           match Q.pop t.q with
           | Some op -> t.container.(!i) <- op; incr i; true
           | None -> false
         do () done;
         let batch = Array.init !i (fun i -> t.container.(i)) in
         par_prefix_sums pool t batch;
         (* let count = Atomic.get t.counter in *)
         (* (Domain.self () :> int) |> Printf.printf "Domain launching %d\n"; *)
         (* Printf.printf "Diff = %d; count = %d\n%!" !i count; *)
         (* par_prefix_sumsv1 pool 0 !i; *)
         Atomic.set t.running false;
         try_launch pool t)
      | None -> Atomic.set t.running false

  let increment pool t =
    let pr, set = T.promise () in
    Q.push t.q (Incr (t, set));
    try_launch pool t;
    T.await pool pr

  let decrement pool t =
    let pr, set = T.promise () in
    Q.push t.q (Decr (t, set));
    try_launch pool t;
    T.await pool pr

  let get pool t =
    let pr, set = T.promise () in
    Q.push t.q (Get (t, set));
    try_launch pool t;
    T.await pool pr
end

module ParCounter : S = struct
  include CounterBase
  let increment _pool t =
    Atomic.incr t.counter

  let decrement _pool t =
    Atomic.decr t.counter

  let get _pool t =
    Atomic.get t.counter

end

module LockCounter : S = struct
  include CounterBase
  let increment _pool t =
    Mutex.lock t.lock;
    Atomic.incr t.counter;
    Mutex.unlock t.lock

  let decrement _pool t =
    Mutex.lock t.lock;
    Atomic.decr t.counter;
    Mutex.unlock t.lock

  let get _pool t =
    Mutex.lock t.lock;
    let res = Atomic.get t.counter in
    Mutex.unlock t.lock;
    res
end
