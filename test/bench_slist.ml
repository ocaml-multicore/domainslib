module T = Domainslib.Task
module type Slist =
  functor (V: Slist.Comparable) ->
  sig
    type t

    val make : size:int -> batch_size:int -> unit -> t
    val search : t -> V.t -> bool
    val seq_ins : t -> V.t -> unit
    val imp_batch_ins : T.pool -> t -> V.t -> unit
    val size : t -> int
    val print_stats : t -> unit
  end

module Bench (SLF : Slist) = struct
  module SL = SLF(Int)

  module T = Domainslib.Task
  let preset_size = 1_000_000
  let additional = 100_000
  let total_size = preset_size + additional
  let max_rdm_int = (Int.shift_left 1 30) - 1
  let preset_arr =
    Random.init 0;
    Array.init preset_size (fun _ -> Random.int max_rdm_int)
  let additional_arr =
    Array.init additional (fun _ -> Random.int max_rdm_int)

  let init () =
    let t = SL.make ~size:total_size ~batch_size:127 () in
    (* Insert Random *)
    Array.iter (fun elt -> SL.seq_ins t elt) preset_arr;
    t

  let with_pool f t num_domains =
    let pool = T.setup_pool ~num_domains () in
    T.run pool (f pool t num_domains) ;
    T.teardown_pool pool

  let test_seq _pool t _num_domains () =
    Array.iter (fun elt -> SL.seq_ins t elt) additional_arr

  let run_seq () =
    for i = 0 to 7 do
      let t = init () in
      let t0 = Unix.gettimeofday () in
      with_pool (test_seq) t i;
      let t1 = Unix.gettimeofday () in
      let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
      Format.printf "  %7s%!"
        (Printf.sprintf "%.0f" op_ms);
    done ;
    Format.printf "@."

  let test_imp_batch pool t _num_domains () =
    T.parallel_for pool ~start:0 ~finish:(additional-1) ~body:(fun i ->
        SL.imp_batch_ins pool t (additional_arr.(i))
      )

  let run_imp_batch () =
    for i = 0 to 7 do
      let t = init () in
      let t0 = Unix.gettimeofday () in
      with_pool (test_imp_batch) t i;
      let t1 = Unix.gettimeofday () in
      let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
      Format.printf "  %7s%!"
        (Printf.sprintf "%.0f" op_ms);
    done ;
    Format.printf "@."
end

module Batched_slist : Slist = functor (V : Slist.Comparable) -> struct
  module SL = Slist.Make(V)
  module Q = Mpmc_queue
  type t = {
    slist : SL.t;
    batch_size : int Atomic.t;
    running : bool Atomic.t;
    q : batch_op Q.t;
    container : batch_op array;
    stats : (int, int) Hashtbl.t
  }
  and
    batch_op =
    | Ins of t * V.t * (unit -> unit)
    | Null

  let make ~size ~batch_size() =
    {slist = SL.make ~size ();
     batch_size = Atomic.make 0;
     running = Atomic.make false;
     q = Q.make ();
     container = Array.make batch_size Null;
     stats = Hashtbl.create 100;
    }

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
         let data = Array.mapi (fun _ op ->
             match op with
             | Ins (_, elt, set) -> set (); elt
             | Null -> failwith "Error") batch in
         SL.par_insert t.slist pool data;
         Atomic.set t.running false;
         try_launch pool t)
      | None -> Atomic.set t.running false

  let seq_ins t elt = SL.insert t.slist elt

  let imp_batch_ins pool t elt =
    let pr, set = T.promise () in
    Q.push t.q (Ins (t, elt, set));
    try_launch pool t;
    T.await pool pr


  let search t = SL.search t.slist
  let size t = SL.size t.slist
  let print_stats t = Hashtbl.iter (fun key value -> Printf.printf "key(%d) -> %d\n" key value) t.stats
end

module Bench_batched_slist =  Bench (Batched_slist)

let () =
  Format.printf "@." ;
  Format.printf "   num_domains: " ;
  for i = 1 to 8 do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  Format.printf "ImpBatch_ins: " ;
  Bench_batched_slist.run_imp_batch ();
  Format.printf "     Seq_ins: " ;
  Bench_batched_slist.run_seq () ;
