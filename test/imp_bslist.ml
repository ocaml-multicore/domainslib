open Domainslib
module BatchedSlist (V : Slist.Comparable) = struct
  module VSL = Slist.Make(V)
  type t = {
    pool : Task.pool;
    slist : VSL.t
  }
  type _ batch_op =
    | Ins : V.t -> unit batch_op
    (* We can even add sequential operations into our batchedDS *)
    | Search : V.t -> bool batch_op
    | Size : int batch_op
    | Print : unit batch_op

  type wrapped_batch_op = Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let create pool = {pool; slist = VSL.make ~size:max_int ()}
  let bop t op_arr num =
    let size = VSL.size t.slist in
    let inserts = Array.make num None in
    let i = ref 0 in
    Array.iter (function 
        | Batched_op (Ins elt, set) -> set (); inserts.(!i) <- Some elt; incr i
        | Batched_op (Search elt, set) -> set @@ VSL.search t.slist elt 
        | Batched_op (Size, set) -> set size
        | Batched_op (Print, set) -> set (); VSL.print_slist t.slist
      ) op_arr;
    let inserts = Array.sub inserts 0 !i |> Array.map (Option.get) in
    VSL.par_insert t.slist t.pool inserts
end

module ImpBatchedSlist (V : Slist.Comparable) = struct
  include Batcher.Make(BatchedSlist(V))
  let insert t elt = batchify t (Ins elt)
  let search t elt = batchify t (Search elt)
  let size t = batchify t (Size)
  let print t = batchify t (Print)
end

let () =
  let module IntImpBatchedSlist = ImpBatchedSlist(Int) in
  let n = 1000 in
  let num_domains = Domain.recommended_domain_count () in
  let pool = Task.setup_pool ~num_domains () in
  let slist1 = IntImpBatchedSlist.create pool in
  let slist2 = IntImpBatchedSlist.create pool in
  Task.run pool (fun () -> 
      Task.parallel_for pool ~start:1 ~finish:n ~body:
        (fun elt -> 
           IntImpBatchedSlist.insert slist1 elt; 
           IntImpBatchedSlist.insert slist2 elt);
      assert (IntImpBatchedSlist.size slist1 = n);
      assert (IntImpBatchedSlist.size slist2 = n);
    );
  Task.teardown_pool pool