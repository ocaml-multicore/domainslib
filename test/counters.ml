module T = Domainslib.Task

let run = try bool_of_string Sys.argv.(1) with _ -> false
let batch_size = 4096
let operations = try int_of_string Sys.argv.(2) with _ -> 10_000_000
(* We switch this on to prevent the overhead of batching from dominating our performance gain *)
let delay_on = try bool_of_string Sys.argv.(3) with _ -> false
let delay () = if delay_on then Unix.sleepf 0.000001

module CounterBase = struct
  module Q = Mpmc_queue
  type t = {
    counter : int Atomic.t;
    batch_size : int Atomic.t;
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

  let stats : (int, int) Hashtbl.t = Hashtbl.create 100 ~random:false

  let create n =
    {counter = Atomic.make 0;
     batch_size = Atomic.make 0;
     running = Atomic.make false;
     q = Q.make ();
     container = Array.make n Null;
     lock = Mutex.create ()}

  let unsafe_get t = Atomic.get t.counter
end

module type S = sig
  include module type of CounterBase
  val increment : T.pool -> t -> unit
  val decrement : T.pool -> t -> unit
  val get : T.pool -> t -> int
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

module LockfreeCounter : S = struct
  (* Instead of using the hardware support for fetch-and-add instruction,
     we simulate how contention degrades as cores increase *)
  include CounterBase
  let rec increment _pool t =
    let v = Atomic.get t.counter in
    if Atomic.compare_and_set t.counter v (v + 1)
    then ()
    else increment _pool t

  let rec decrement _pool t =
    let v = Atomic.get t.counter in
    if Atomic.compare_and_set t.counter v (v - 1)
    then ()
    else decrement _pool t

  let get _pool t =
    Atomic.get t.counter

end

module BatchedCounter = struct
  include CounterBase

  let par_prefix_sums pool t arr =
    let len = Array.length arr in
    let start = Atomic.get t.counter in
    let add_n = T.parallel_for_reduce pool ~start:0 ~finish:(len-1)
        ~body:(fun i ->
            delay ();
            match arr.(i) with
            | Incr (_, set) ->  set (); 1
            | Decr (_, set) ->  set (); -1
            | Get (_, set) -> set start; 0
            | Null -> failwith "Bad") ( + ) 0 in
    Atomic.set t.counter (start + add_n)

  let rec try_launch pool t =
    if Atomic.get t.batch_size > 0
    && Atomic.compare_and_set t.running false true then
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
         (match Hashtbl.find_opt stats !i with
          | Some cnt -> Hashtbl.replace stats !i (cnt + 1)
          | None -> Hashtbl.add stats !i 1);
         Atomic.set t.running false;
         try_launch pool t)
      | None -> Atomic.set t.running false

  let increment pool t =
    let pr, set = T.promise () in
    Q.push t.q (Incr (t, set));
    Atomic.incr t.batch_size;
    try_launch pool t;
    T.await pool pr

  let decrement pool t =
    let pr, set = T.promise () in
    Q.push t.q (Decr (t, set));
    Atomic.incr t.batch_size;
    try_launch pool t;
    T.await pool pr

  let get pool t =
    let pr, set = T.promise () in
    Q.push t.q (Get (t, set));
    Atomic.incr t.batch_size;
    try_launch pool t;
    T.await pool pr
end

module BatchedCounterFast : S = struct
  include CounterBase

  let rec try_launch pool t =
    if Atomic.get t.batch_size > 0
    && Atomic.compare_and_set t.running false true
    then
      let len = Atomic.exchange t.batch_size 0 in
      (match Hashtbl.find_opt stats len with
       | Some cnt -> Hashtbl.replace stats len (cnt + 1)
       | None -> Hashtbl.add stats len 1);
      let cur = Atomic.get t.counter in
      assert (len > 0);
      let add_n =
        T.parallel_for_reduce pool ~start:0 ~finish:(len-1)
          ~body:(fun _i ->
              delay ();
              match Q.pop t.q |> Option.get with (* no [batch] or [t.container] array *)
              | Incr (_, set) -> set (); 1
              | Decr (_, set) ->  set (); -1
              | Get (_, set) -> set cur; 0
              | Null -> failwith "Bad") ( + ) 0 in
      Atomic.set t.counter (cur + add_n);
      Atomic.set t.running false;
      try_launch pool t

  let increment pool t =
    let pr, set = T.promise () in
    Q.push t.q (Incr (t, set));
    Atomic.incr t.batch_size;
    try_launch pool t;
    T.await pool pr

  let decrement pool t =
    let pr, set = T.promise () in
    Q.push t.q (Decr (t, set));
    Atomic.incr t.batch_size;
    try_launch pool t;
    T.await pool pr

  let get pool t =
    let pr, set = T.promise () in
    Q.push t.q (Get (t, set));
    Atomic.incr t.batch_size;
    try_launch pool t;
    T.await pool pr
end

module BatchedCounterFaster : S = struct
  include CounterBase

  let rec try_launch pool t =
    if Atomic.get t.batch_size > 0
    && Atomic.compare_and_set t.running false true
    then
      let len = Atomic.exchange t.batch_size 0 in
      (* assert (len > 0); *)
      let add_n =
        T.parallel_for_reduce pool ~start:0 ~finish:(len-1)
          ~body:(fun _ ->
              delay ();
              match Q.pop t.q |> Option.get with (* no [batch] or [t.container] array *)
              | Incr (_, set) -> set (); 1
              | Decr (_, set) ->  set (); -1
              | _ -> failwith "Bad") ( + ) 0 in
      let _ : int = Atomic.fetch_and_add t.counter add_n in
      Atomic.set t.running false;
      try_launch pool t

  let increment pool t =
    if (* fast path *)
      let v = Atomic.get t.counter in
      Atomic.compare_and_set t.counter v (v + 1)
    then ()
    else begin (* slow path *)
      let pr, set = T.promise () in
      Q.push t.q (Incr (t, set));
      Atomic.incr t.batch_size;
      try_launch pool t;
      T.await pool pr
    end

  let decrement pool t =
    if (* fast path *)
      let v = Atomic.get t.counter in
      Atomic.compare_and_set t.counter v (v - 1)
    then ()
    else begin (* slow path *)
      let pr, set = T.promise () in
      Q.push t.q (Decr (t, set));
      Atomic.incr t.batch_size;
      try_launch pool t;
      T.await pool pr
    end

  let get _pool t = Atomic.get t.counter
end

let run_stats () =
  let open BatchedCounter in
  let chunk_size = Util.chunk_calculator ~batch_size ~operations () in
  let t = create operations in
  let pool = T.setup_pool ~num_domains:7 () in
  T.run pool (fun () ->
      T.parallel_for pool ~chunk_size ~start:1 ~finish:operations ~body:
        (fun _ -> increment pool t)
    );
  Util.print_implicit_batch_stats stats;
  T.teardown_pool pool

let () =   
  if run then 
    begin 
      Printf.printf "\nRunning ImpBatchCounter Statistics, batch_size = %d, ops = %d\n%!" batch_size operations;
      run_stats ()
    end;