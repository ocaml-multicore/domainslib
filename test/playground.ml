module Batcher = Domainslib.Batcher
module T = Domainslib.Task
let pool = T.setup_pool ~num_domains:7 ()

module BatchedCounter = struct
  type t = int Atomic.t
  type batch_op =
    | Incr of (unit -> unit)
    | Decr of (unit -> unit)
    | Get of (int -> unit)

  let batch_limit = 4096
  let pool = pool
  let create () = Atomic.make 0
  let bop t arr _num =
    let len = Array.length arr in
    let start = Atomic.get t in
    let add_n = T.parallel_for_reduce pool ~start:0 ~finish:(len-1)
        ~body:(fun i -> match arr.(i) with
            | Incr set ->  set (); 1
            | Decr set ->  set (); -1
            | Get set -> set start; 0) ( + ) 0 
    in
    Atomic.set t (start + add_n)

end

module ImpBatchedCounter = struct 
  include Batcher.Make(BatchedCounter)

  let increment () =
    let pr, set = T.promise () in
    batchify (Incr set) pr

  let decrement () =
    let pr, set = T.promise () in
    batchify (Decr set) pr

  let get () =
    let pr, set = T.promise () in
    batchify (Get set) pr

end

let main () =
  T.parallel_for pool ~chunk_size:1 ~start:1 ~finish:1_000_000 ~body:
    (fun _ -> ImpBatchedCounter.increment (); ImpBatchedCounter.decrement ();
      Printf.printf "Got %d\n" @@ ImpBatchedCounter.get ())

let () = 
  T.run pool main;
  T.teardown_pool pool 