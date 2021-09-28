let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 43

module T = Domainslib.Task

let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let rec fib_par n =
  if n <= 40 then fib n
  else
    let a = T.async (fun _ -> fib_par (n-1)) in
    let b = T.async (fun _ -> fib_par (n-2)) in
    T.await a + T.await b

let main =
  let res = fib_par n in
  T.Pool.teardown_default_pool ();
  Printf.printf "fib(%d) = %d\n" n res
