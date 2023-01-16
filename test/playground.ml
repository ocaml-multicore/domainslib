module Batcher = Domainslib.Batcher
module T = Domainslib.Task
let delay () = Unix.sleepf 0.000001
module BatchedCounter = struct
  type t = {
    counter : int Atomic.t;
    pool : T.pool
  }

  type batch_op =
    | Incr of (unit -> unit)
    | Decr of (unit -> unit)
    | Get of (int -> unit)

  let create pool () = {counter = Atomic.make 0; pool}

  let bop t arr _num =
    let len = Array.length arr in
    let start = Atomic.get t.counter in
    let add_n = T.parallel_for_reduce t.pool ~start:0 ~finish:(len-1)
        ~body:(fun i -> 
            delay ();
            match arr.(i) with
            | Incr set ->  set (); 1
            | Decr set ->  set (); -1
            | Get set -> set start; 0) ( + ) 0 
    in
    Atomic.set t.counter (start + add_n)

end

module ImpBatchedCounter = struct 
  include Batcher.Make(BatchedCounter)

  let increment t =
    let pr, set = T.promise () in
    batchify t (Incr set) pr

  let decrement t =
    let pr, set = T.promise () in
    batchify t (Decr set) pr

  let get t =
    let pr, set = T.promise () in
    batchify t (Get set) pr

end

let main () =
  let n = 100_000 in
  for num_domains = 0 to 7 do
    let pool = T.setup_pool ~num_domains () in
    let t = ImpBatchedCounter.create pool in
    let t0 = Unix.gettimeofday () in
    T.run pool (fun () ->
        T.parallel_for pool ~chunk_size:(n/4096) ~start:1 ~finish:n ~body:(fun _ -> ImpBatchedCounter.increment t)
      );
    assert (ImpBatchedCounter.get t = n);
    let t1 = Unix.gettimeofday () in
    T.teardown_pool pool;
    Format.printf "  %7s%!"
      (Printf.sprintf "%.2f" (1000.0 *. (t1 -. t0)))
  done ;
  Format.printf "@."

let () = 
  Format.printf "@." ;
  Format.printf "        num_domains: " ;
  for i = 1 to 8 do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  Format.printf " BatchCounterFunctor: " ;
  main ()
