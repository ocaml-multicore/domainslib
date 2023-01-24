module T = Domainslib.Task
module ISL = Slist.Make(Int)

let max_rdm_int = (Int.shift_left 1 30) - 1
let usage_msg = "-num_domains <7> -preset <1_000_000> -additional <100_000>"
let num_domains = ref (Domain.recommended_domain_count () - 1)
let preset = ref 1_000_000
let additional = ref 100_000
let total_size = ref (!preset + !additional)
let speclist =
  [
    ("-num_domains", Arg.Set_int num_domains, "Set number of additional domains");
    ("-preset", Arg.Set_int preset, "Set number of preset elements");
    ("-additional", Arg.Set_int additional, "Set number of inserts")
  ]

let set_size preset_arr additional_arr =
  let t = ISL.make ~size:!total_size () in
  Array.iter (fun elt -> ISL.insert t elt) preset_arr;
  Array.iter (fun elt -> ISL.insert t elt) additional_arr;
  ISL.size t

let run num_domains preset_arr additional_arr set_size = 
  let pool = T.setup_pool ~num_domains () in
  let t = ISL.make ~size:!total_size () in
  (* Insert preset elements *)
  Array.iter (fun elt -> ISL.insert t elt) preset_arr;
  Gc.full_major ();
  let t0 = Unix.gettimeofday () in
  (* Perform batch parallel insertions *)
  T.run pool (fun () -> ISL.par_insert t pool additional_arr);
  let t1 = Unix.gettimeofday () in
  assert(ISL.size t = set_size); 
  let op_ms = (Int.to_float !additional) /. (1000.0 *. (t1 -. t0)) in
  Format.printf "  %7s%!" (Printf.sprintf "%.0f" op_ms);
  T.teardown_pool pool

let main () =
  let preset_arr = Array.init !preset (fun _ -> Random.int max_rdm_int) in
  let additional_arr = Array.init !additional (fun _ -> Random.int max_rdm_int)in
  let set_size = set_size preset_arr additional_arr in
  Format.printf "@." ;
  Format.printf "num_domains: " ;
  for i = 1 to !num_domains+1 do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  Format.printf "Batch_ins: " ;
  for num_domains = 0 to !num_domains do
    run num_domains preset_arr additional_arr set_size
  done

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  total_size := !preset + !additional;
  Printf.printf "\n\nRunning BatchSlist inserts, preset=%d, inserts=%d\n%!" !preset !additional;
  main (); print_newline ()

