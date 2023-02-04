open Domainslib
module BatchedCounter = struct
  type t = int Atomic.t

  type 'a batch_op =
    | Incr : unit batch_op
    | Decr : unit batch_op
    | Get : int batch_op
  type wrapped_batch_op = Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let create () = Atomic.make 0

  let bop t pool (arr:wrapped_batch_op array) _num =
    let len = Array.length arr in
    let start : int = Atomic.get t in
    let add_n = Task.parallel_for_reduce pool ~start:0 ~finish:(len-1)
        ~body:(fun i -> 
            match arr.(i) with
            | Batched_op (Incr, set) ->  set (); 1
            | Batched_op (Decr, set) ->  set (); -1
            | Batched_op (Get, set) -> set start; 0) ( + ) 0 
    in
    Atomic.set t (start + add_n)

end

module ImpBatchedCounter = struct 
  include Batcher.Make(BatchedCounter)
  let increment t = batchify t Incr
  let [@warning "-32"]decrement t = batchify t Decr
  let get t = batchify t Get

end

let () =
  let n = 100_000 in
  let num_domains = Domain.recommended_domain_count () in
  let pool = Task.setup_pool ~num_domains () in
  let t1 = ImpBatchedCounter.create pool in
  let t2 = ImpBatchedCounter.create pool in
  Task.run pool (fun () ->
      Task.parallel_for pool ~chunk_size:(n/4096) ~start:1 ~finish:n ~body:(fun _ -> ImpBatchedCounter.increment t1; ImpBatchedCounter.increment t2)
    );
  assert (ImpBatchedCounter.get t1 = n);
  assert (ImpBatchedCounter.get t2 = n);
  Task.teardown_pool pool
