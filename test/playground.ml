module Batcher = Domainslib.Batcher
module T = Domainslib.Task
(*let delay () = Unix.sleepf 0.000001 *)
module BatchedCounter = struct
  type t = {
    counter : int Atomic.t;
    pool : T.pool
  }

  type 'a batch_op =
    | Incr : unit batch_op
    | Decr : unit batch_op
    | Get : int batch_op
  type wrapped_batch_op = Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let create pool () = {counter = Atomic.make 0; pool}

  let bop t (arr:wrapped_batch_op array) _num =
    let len = Array.length arr in
    let start = Atomic.get t.counter in
    let add_n = T.parallel_for_reduce t.pool ~start:0 ~finish:(len-1)
        ~body:(fun i -> 
            (*delay (); *)
            match arr.(i) with
            | Batched_op (Incr, set) ->  set (); 1
            | Batched_op (Decr, set) ->  set (); -1
            | Batched_op (Get, set) -> set start; 0) ( + ) 0 
    in
    Atomic.set t.counter (start + add_n)

end

(* Make operations require a pool parameter *)
module ImpBatchedCounter = struct 
  include Batcher.Make(BatchedCounter)

  let increment t = batchify t Incr

  let decrement t = batchify t Decr

  let get t = batchify t Get

end

let main () =
  let n = 100_000 in
  for num_domains = 0 to 7 do
    let pool = T.setup_pool ~num_domains () in
    let t = ImpBatchedCounter.create pool in
    let t2 = ImpBatchedCounter.create pool in

    let t0 = Unix.gettimeofday () in
    T.run pool (fun () ->
        T.parallel_for pool ~chunk_size:(n/4096) ~start:1 ~finish:n ~body:(fun _ -> ImpBatchedCounter.increment t; ImpBatchedCounter.increment t2)
      );
    assert (ImpBatchedCounter.get t = n);
    assert (ImpBatchedCounter.get t2 = n);
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
