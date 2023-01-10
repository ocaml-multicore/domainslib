module T = Domainslib.Task

module Batched_ISL = struct
  module ISL = Slist.Make (Int)
  module Q = Mpmc_queue

  type t = {
    slist : ISL.t;
    batch_size : int Atomic.t;
    running : bool Atomic.t;
    q : batch_op Q.t;
    container : batch_op array
  }
  and
    batch_op =
    | Ins of t * int * (unit -> unit)
    | Null

  let create size batch_size = {
    slist = ISL.make ~size ();
    batch_size = Atomic.make 0;
    running = Atomic.make false;
    q = Q.make ();
    container = Array.make batch_size Null
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
         let batch = Array.init !i (fun i ->
             match t.container.(i) with
             | Ins (_, elt, set) -> set (); elt
             | Null -> failwith "Bad"
           ) in
         ISL.par_insert t.slist pool batch;
         Atomic.set t.running false;
         try_launch pool t)
      | None -> Atomic.set t.running false

  let seq_insert t elt = ISL.insert t.slist elt

  let impbatch_insert t pool elt =
    let pr, set = T.promise () in
    Q.push t.q (Ins (t, elt, set));
    try_launch pool t;
    T.await pool pr

  let batch_insert t pool arr = ISL.par_insert t.slist pool arr
end

module Bench = struct
  let preset_size = 1_000_000
  let additional = 100_000
  let total_size = preset_size + additional
  let max_rdm_int = (Int.shift_left 1 30) - 1
  let preset_arr =  Random.init 0;
    Array.init preset_size (fun _ -> Random.int max_rdm_int)
  let additional_arr =
    Array.init additional (fun _ -> Random.int max_rdm_int)

  let init () =
    let t = Batched_ISL.create total_size 127 in
    Array.iter (fun elt -> Batched_ISL.seq_insert t elt) preset_arr;
    t

  let run_seq () =
    for _ = 0 to 7 do
      let t = init () in
      let t0 = Unix.gettimeofday () in
      Array.iter (fun elt -> Batched_ISL.seq_insert t elt) additional_arr;
      let t1 = Unix.gettimeofday () in
      let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
      Format.printf "  %7s%!" (Printf.sprintf "%.0f" op_ms);
    done

  let run_batch () =
    for num_domains = 0 to 7 do
      let pool = T.setup_pool ~num_domains () in
      let t = init () in
      Gc.full_major ();
      let t0 = Unix.gettimeofday () in
      T.run pool (fun () -> Batched_ISL.batch_insert t pool additional_arr);
      let t1 = Unix.gettimeofday () in
      let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
      Format.printf "  %7s%!" (Printf.sprintf "%.0f" op_ms);
      T.teardown_pool pool
    done

  let run_impbatch () =
    for num_domains = 0 to 7 do
      let pool = T.setup_pool ~num_domains () in
      let t = init () in
      Gc.full_major ();
      let t0 = Unix.gettimeofday () in
      T.run pool (fun () ->
          T.parallel_for pool ~chunk_size:(additional/127) ~start:0 ~finish:(additional-1) ~body:(fun i ->
              Batched_ISL.impbatch_insert t pool additional_arr.(i)));
      let t1 = Unix.gettimeofday () in
      let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
      Format.printf "  %7s%!" (Printf.sprintf "%.0f" op_ms);
      T.teardown_pool pool
    done


end

let () =
  Format.printf "@." ;
  Format.printf "   num_domains: " ;
  for i = 1 to 8 do
    Format.printf " %5i   " i
  done ;
  (* Format.printf "@." ; *)
  (* Format.printf "     Seq_ins: " ; *)
  (* Bench.run_seq (); *)
  (* Format.printf "@." ; *)
  (* Format.printf "   Batch_ins: " ; *)
  (* Bench.run_batch (); *)
  Format.printf "@." ;
  Format.printf "ImpBatch_ins: " ;
  Bench.run_impbatch ()
