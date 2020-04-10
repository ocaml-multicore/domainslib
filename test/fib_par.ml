let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 40

module T = Domainslib.Task

let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let rec fib_par pool n =
  if n <= 40 then fib n
  else
    let r = T.make_reducer (+) 0 in
    let a = T.async pool r (fun _ -> fib_par pool (n-1)) in
    let b = T.async pool r (fun _ -> fib_par pool (n-2)) in
    T.await a + T.await b

let main =
  let pool = T.setup_pool ~num_domains:(num_domains - 1) in
  let res = fib_par pool n in
  T.teardown_pool pool;
  Printf.printf "fib(%d) = %d" n res
