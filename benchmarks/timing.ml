
let progress_bar ~total =
  let open Progress.Line in
  list [spinner (); bar ~width:(`Expand) total; count_to total]

let step ?(show_progress=false) ~name ~total f =
  if show_progress then begin
    Format.printf "Running %s\n%!" name;
    Progress.with_reporter (progress_bar ~total:total) f
  end else f (fun _ -> ())
    

let time ?show_progress ~no_warmup ~no_iter ~init f =

  step ?show_progress ~name:"warmup" ~total:no_warmup (fun prog ->
    for _ = 1 to no_warmup do let state = init () in f state; prog 1 done;
  );

  let sum = ref 0. in
  let sum_sq = ref 0. in

  let count = ref 0. in
  let mean = ref 0. in
  let m2 = ref 0. in


  step ?show_progress ~name:"benchmark" ~total:no_iter (fun prog -> 
    for _ = 1 to no_iter do

      let state = init () in

      let start_time = Ptime_clock.now () in
      f state;
      let end_time = Ptime_clock.now () in
      let time = Ptime.Span.to_float_s (Ptime.diff end_time start_time) in

      sum := !sum +. time;
      sum_sq := !sum_sq +. (time *. time);

      (* walford's algorithm *)
      count := !count +. 1.;
      let delta = time -. !mean in
      mean := !mean +. (delta /. !count);
      let delta2 = time -. !mean in
      m2 := !m2 +. delta *. delta2;

      prog 1
    done
  );

  let var = (!sum_sq -. (!sum *. !sum)/.(Float.of_int no_iter)) /. (Float.of_int (no_iter - 1)) in
  let sd = Float.sqrt var in

  let mean, _variance, sample_variance = !mean, !m2 /. !count, !m2 /. (!count -. 1.0) in
  let sample_sd = Float.sqrt sample_variance in

  let avg_time = !sum /. Float.of_int no_iter in

  if !count <= 2.0
  then Format.printf "%.5fs ± %.5fs\n%!" avg_time sd
  else Format.printf "%.5fs ± %.5fs\n%!" mean sample_sd
