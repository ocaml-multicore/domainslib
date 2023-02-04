let time f =
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  Format.printf "@[@;Runtime: %.2fms@]@." ((t1 -. t0) *. 1000.)