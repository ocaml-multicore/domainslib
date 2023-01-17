module T = Domainslib.Task
module type Slist = module type of Slist.MakeImpBatched
let preset_size = try int_of_string Sys.argv.(2) with _ -> 1_000_000
let additional = 100_000
let batch_size = 127

module Bench (SLF : Slist) = struct
  module SL = SLF(Int)

  let total_size = preset_size + additional
  let max_rdm_int = (Int.shift_left 1 30) - 1
  let preset_arr =
    Random.init 0;
    Array.init preset_size (fun _ -> Random.int max_rdm_int)
  let additional_arr =
    Array.init additional (fun _ -> Random.int max_rdm_int)

  let init () =
    let t = SL.make ~size:total_size ~batch_size () in
    (* Insert Random *)
    Array.iter (fun elt -> SL.seq_ins t elt) preset_arr;
    t

  let run f =
    for num_domains = 0 to 7 do
      let pool = T.setup_pool ~num_domains () in
      let t = init () in
      let t0 = Unix.gettimeofday () in
      T.run pool (f t pool);
      let t1 = Unix.gettimeofday () in
      let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
      Format.printf "  %7s%!" (Printf.sprintf "%.0f" op_ms);
      T.teardown_pool pool
    done ;
    Format.printf "@."

  let run_seq () = 
    let test_seq t _pool () =
      Array.iter (fun elt -> SL.seq_ins t elt) additional_arr 
    in
    run test_seq

  let run_batch () =
    let test_batch t pool () = SL.batch_ins t pool additional_arr
    in
    run test_batch

  let run_imp_batch () =
    let test_imp_batch t pool () =
      let chunk_size = Util.chunk_calculator ~batch_size ~operations:additional () in
      T.parallel_for pool ~chunk_size ~start:0 ~finish:(additional-1) ~body:(fun i ->
          SL.imp_batch_ins t pool (additional_arr.(i)))
    in
    run test_imp_batch
end

module Bench_batched_slist =  Bench (Slist.MakeImpBatched)

let () =
  Printf.printf "\n\nRunning ImpBatch Slist Benchmarks, batch_size = %d, preset = %d, additional inserts = %d\n%!" batch_size preset_size additional;
  Format.printf "@." ;
  Format.printf "   num_domains: " ;
  for i = 1 to 8 do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  (* Why is there such a big difference when adding this? *)
  (* Format.printf "   Batch_ins: " ;
     Bench_batched_slist.run_batch (); *)
  Format.printf "     Seq_ins: " ;
  Bench_batched_slist.run_seq () ;
  Format.printf "ImpBatch_ins: " ;
  Bench_batched_slist.run_imp_batch ();
