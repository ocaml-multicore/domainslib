[@@@alert "-unstable"]
open Domainslib
module BatchedCounter = struct
  type t = int Atomic.t

  type 'a op =
    | Incr : unit op
    | Decr : unit op
    | Get : int op
  type wrapped_op = Mk : 'a op * ('a -> unit) -> wrapped_op

  let init () = Atomic.make 0

  let run t pool (arr:wrapped_op array)  =
    let len = Array.length arr in
    let start : int = Atomic.get t in
    let add_n = Task.parallel_for_reduce pool ~start:0 ~finish:(len-1)
        ~body:(fun i -> 
            match arr.(i) with
            | Mk (Incr, set) ->  set (); 1
            | Mk (Decr, set) ->  set (); -1
            | Mk (Get, set) -> set start; 0) ( + ) 0 
    in
    Atomic.set t (start + add_n)

end

module ImpBatchedCounter = struct 
  include Batcher.Make(BatchedCounter)
  let increment t = apply t Incr
  let [@warning "-32"]decrement t = apply t Decr
  let get t = apply t Get

end

let () =
  let n = 100_000 in
  let num_domains = Domain.recommended_domain_count () in
  let pool = Task.setup_pool ~num_domains () in
  let t1 = ImpBatchedCounter.init pool in
  let t2 = ImpBatchedCounter.init pool in
  Task.run pool (fun () ->
      Task.parallel_for pool ~chunk_size:(n/4096) ~start:1 ~finish:n ~body:(fun _ -> ImpBatchedCounter.increment t1; ImpBatchedCounter.increment t2)
    );
  assert (ImpBatchedCounter.get t1 = n);
  assert (ImpBatchedCounter.get t2 = n);
  Task.teardown_pool pool
