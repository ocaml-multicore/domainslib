
let n_iterations = try int_of_string Sys.argv.(1) with _ -> 1024
let n_tasks = try int_of_string Sys.argv.(2) with _ -> 1024
let n_domains =
  try int_of_string Sys.argv.(3)
  with _ -> Domain.recommended_domain_count ()

module T = Domainslib.Task

module TimingHist = struct
  type t = {
    data: int array;
    min_n: int;
    max_n: int;
    mutable count: int;
    mutable sum  : float;
    }

  let make min_n max_n =
    { data=Array.make (max_n - min_n) 0; min_n; max_n; count=0; sum=0. }

  let rec log2 n =
    if n <= 1 then 0 else 1 + log2(n asr 1)

  let add_point a x =
    let i = (log2 x) in
    let i = max (i-a.min_n+1) 0 in
    let i = min i ((Array.length a.data)-1) in
    a.data.(i) <- a.data.(i) + 1;
    a.sum <- a.sum +. (float_of_int x);
    a.count <- a.count + 1

  let mean a =
    a.sum /. (float_of_int a.count)

  let print_hist a =
    Printf.printf "Timings (ns): n=%d  mean=%.1f\n" a.count (mean a);
    let fn n = (Int.shift_left 1 (a.min_n+n)) in
    let len = Array.length a.data in
    for i = 0 to (len - 1) do
      match i with
      | i when i=0 ->
        Printf.printf " (%8d, %8d): %6d\n" 0 (fn i) a.data.(i);
      | i when i=(len-1) ->
        Printf.printf " [%8d,      Inf): %6d\n" (fn (i-1)) a.data.(i);
      | i ->
        Printf.printf " [%8d, %8d): %6d\n" (fn (i-1)) (fn i) a.data.(i);
    done

end

let _ =
  Printf.printf "n_iterations: %d   n_units: %d  n_domains: %d\n"
    n_iterations n_tasks n_domains;
  let pool = T.setup_pool ~num_domains:(n_domains - 1) () in

  let hist = TimingHist.make 5 25 in
  for _ = 1 to n_iterations do
    let t0 = Mclock.elapsed_ns() in
    T.run pool (fun _ ->
      T.parallel_for pool ~start:1 ~finish:n_tasks ~body:(fun _ -> ()));
    let t = Int64.sub (Mclock.elapsed_ns ()) t0 in
    TimingHist.add_point hist (Int64.to_int t);
  done;

  TimingHist.print_hist hist;

  T.teardown_pool pool
