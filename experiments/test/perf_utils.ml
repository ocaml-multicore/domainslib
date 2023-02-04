module T = Domainslib.Task

let run f num_domains =
  let pool = T.setup_pool ~num_domains () in
  let t0 = Unix.gettimeofday () in
  let _tree = T.run pool (f pool) in
  let t1 = Unix.gettimeofday () in
  let time = (t1 -. t0) *. 1000. in
  Format.printf "Time to build = %.2f ms\n" time;
  T.teardown_pool pool

