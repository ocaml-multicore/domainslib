open Domainslib
module BatchedSlist (V : Slist.Comparable) = struct
  module VSL = Slist.Make(V)
  type t = VSL.t

  type _ op =
    | Ins : V.t -> unit op
    (* We can even add sequential operations into our batchedDS *)
    | Search : V.t -> bool op
    | Size : int op
    | Print : unit op

  type wrapped_op = Mk : 'a op * ('a -> unit) -> wrapped_op

  let init () = VSL.make ~size:max_int ()
  let run t pool op_arr =
    let size = VSL.size t in
    let inserts = Array.make (Array.length op_arr) None in
    let i = ref 0 in
    Array.iter (function 
        | Mk (Ins elt, set) -> set (); inserts.(!i) <- Some elt; incr i
        | Mk (Search elt, set) -> set @@ VSL.search t elt 
        | Mk (Size, set) -> set size
        | Mk (Print, set) -> set (); VSL.print_slist t
      ) op_arr;
    let inserts = Array.sub inserts 0 !i |> Array.map (Option.get) in
    VSL.par_insert t pool inserts

end

module ImpBatchedSlist (V : Slist.Comparable) = struct
  include Batcher.Make(BatchedSlist(V))
  let insert t elt = apply t (Ins elt)
  let search t elt = apply t (Search elt)
  let size t = apply t (Size)
  let print t = apply t (Print)
end

module IntImpBatchedSlist = ImpBatchedSlist(Int)


let test_singular () = 
  let n = 10000 in
  let num_domains = Domain.recommended_domain_count () - 1 in
  let pool = Task.setup_pool ~num_domains () in
  Task.run pool (fun () ->
      for _ = 1 to 10 do
        let slist = IntImpBatchedSlist.init pool in
        Task.parallel_for pool ~start:1 ~finish:n ~body:
          (fun elt -> IntImpBatchedSlist.insert slist elt);
        assert (IntImpBatchedSlist.size slist = n)
      done
    );
  Task.teardown_pool pool

let test_multiple () =
  let n = 10000 in
  let num_domains = Domain.recommended_domain_count () - 1 in
  let pool = Task.setup_pool ~num_domains () in
  let slist_arr = Array.init 10 (fun _ -> IntImpBatchedSlist.init pool) in
  Task.run pool (fun () ->
      Task.parallel_for pool ~start:1 ~finish:n ~body:
        (fun elt ->
           Array.iter (fun slist -> IntImpBatchedSlist.insert slist elt) slist_arr);
      Array.iter (fun slist -> assert(IntImpBatchedSlist.size slist = n)) slist_arr;
    );
  Task.teardown_pool pool

let test_batch_insert () =
  let module IBSlist = Slist.Make(Int) in
  let preset = 1_000_000 in
  let n = 100_000 in
  let max_rdm_int = (Int.shift_left 1 30) - 1 in
  let preset_arr = Array.init preset (fun _ -> Random.int max_rdm_int) in
  let additional_arr = Array.init preset (fun _ -> Random.int max_rdm_int) in
  let num_domains = Domain.recommended_domain_count () - 1 in
  let pool = Task.setup_pool ~num_domains () in
  let t = IBSlist.make ~size:(preset + n) () in
  Task.run pool (fun () ->
      IBSlist.par_insert t pool preset_arr;
      IBSlist.par_insert t pool additional_arr
    );
  Task.teardown_pool pool

let () = test_singular ()
