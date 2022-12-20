(* module Atomic = struct
   include Atomic
   let get t = Unix.sleepf 0.1; Atomic.get t
   let incr t = Unix.sleepf 0.1; Atomic.incr t
   let decr t = Unix.sleepf 0.1; Atomic.decr t
   let fetch_and_add t = Unix.sleepf 0.1; Atomic.fetch_and_add t
   end *)

module T = Domainslib.Task

module type CounterBaseS = sig
  module Q = Mpmc_queue
  type t =  {
    counter : int Atomic.t; 
    running : bool Atomic.t; 
    lock : Mutex.t;
    (* lo : int Atomic.t; 
       hi : int Atomic.t; *)
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
    (* lo : int Atomic.t; 
       hi : int Atomic.t; *)
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
    {counter = Atomic.make 0; running = Atomic.make false; (*lo = Atomic.make 0; hi = Atomic.make 0;*) q = Q.make (); container = Array.make n Null; lock = Mutex.create ()}

  let unsafe_get t = Atomic.get t.counter
end

module type Counter = sig
  include CounterBaseS
  val increment : T.pool -> t -> unit
  val decrement : T.pool -> t -> unit
  val get : T.pool -> t -> int
end

module BC_MPMC : Counter = struct
  include CounterBase
  let _par_prefix_sumsv1 pool t arr =
    let convert : batch_op -> int = function
      | Incr _ -> 1
      | Decr _ -> -1
      | Get _ -> 0
      | Null -> failwith "Impossible" in
    let converted = Array.map convert arr in
    let res = T.parallel_scan pool ( + ) converted in
    let len = Array.length arr in
    let add_n = res.(len-1) in
    let n = Atomic.fetch_and_add t.counter add_n + add_n in
    T.parallel_for pool ~start:0 ~finish:(len-1) 
      ~body:(fun i -> 
          match arr.(i) with
          | Incr (_, set) -> set ()
          | Decr (_, set) -> set ()
          | Get (_, set) -> set n
          | Null -> ())

  let _par_prefix_sumsv2 pool t arr =
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

  let par_prefix_sums = _par_prefix_sumsv2
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


(* module BCArray : Counter = struct

   include CounterBase
   let eval : batch_op -> unit = function
    | Incr (t, set) -> Atomic.incr t.counter |> set
    | Decr (t, set) -> Atomic.decr t.counter |> set
    | Get (t, set) -> Atomic.get t.counter |> set
    | Null -> ()

   let par_prefix_sums pool t lo hi =
    T.parallel_for pool  ~start:lo ~finish:hi ~body:
      (fun i -> eval t.container.(i); t.container.(i) <- Null)

   let rec try_launch pool t =
    let lo, hi = Atomic.get t.lo, Atomic.get t.hi in
    (assert (lo <= hi));
    if lo <> hi then 
      (if Atomic.compare_and_set t.running false true then
         begin 
           par_prefix_sums pool t lo hi;
           let diff = hi - lo in
           (Domain.self () :> int) |> Printf.printf "Domain launching %d\n";
           Printf.printf "Diff = %d | lo = %d, hi = %d |\n%!" diff lo hi;
           Atomic.set t.lo hi; Atomic.set t.running false;
           try_launch pool t
         end)

   let increment pool t =
    let pr, set = T.promise () in
    let idx = Atomic.fetch_and_add t.hi 1 in
    t.container.(idx) <- Incr (t, set);
    try_launch pool t;
    T.await pool pr

   let decrement pool t =
    let pr, set = T.promise () in
    let idx = Atomic.fetch_and_add t.hi 1 in
    t.container.(idx) <- Decr (t, set);
    try_launch pool t;
    T.await pool pr

   let get pool t =
    let pr, set = T.promise () in
    let idx = Atomic.fetch_and_add t.hi 1 in
    t.container.(idx) <- Get (t, set);
    try_launch pool t;
    T.await pool pr

   end *)

module ParCounter : Counter = struct
  include CounterBase
  let increment _pool t =
    Atomic.incr t.counter

  let decrement _pool t =
    Atomic.decr t.counter

  let get _pool t =
    Atomic.get t.counter

end

module LockCounter : Counter = struct
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
(* 
let () =
   let open BCArray in
   let n = 10_000 in
   let t = create n in
   let pool = T.setup_pool ~num_domains:7 () in
   T.run pool (fun () -> 
      T.parallel_for pool ~start:1 ~finish:n ~body:(fun _ -> increment pool t)
    );
   Printf.printf "Size = %d" (unsafe_get t);
   T.teardown_pool pool *)
