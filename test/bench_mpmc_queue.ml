module T = Domainslib.Task
module Q = Mpmc_queue

let nb = 1_000_000

module type CHAN = sig
  type 'a t
  val make : unit -> 'a t
  val send : 'a t -> 'a -> unit
  val recv : pool:T.pool -> 'a t -> 'a
end

module Bench (C : CHAN) = struct

  let test ~pool () =
    let t = C.make () in
    let _ =
      T.async pool @@ fun () ->
      T.parallel_for pool ~start:1 ~finish:nb ~body:(fun i ->
        C.send t i
      )
    in
    T.parallel_for pool ~start:1 ~finish:nb ~body:(fun _ ->
      ignore (C.recv ~pool t)
    )

  let run num_domains =
    let pool = T.setup_pool ~num_domains () in
    T.run pool (test ~pool) ;
    T.teardown_pool pool

  let run () =
    for i = 1 to 8 do
      let t0 = Unix.gettimeofday () in
      run i ;
      let t1 = Unix.gettimeofday () in
      Format.printf "  %7s%!"
        (Printf.sprintf "%.2f" (1000.0 *. (t1 -. t0)))
    done ;
    Format.printf "@."

end

module Bench_mpmc_pool = Bench (struct
  module Q = Mpmc_queue
  type 'a t = 'a Q.t
  let make () = Q.make ()
  let send t v = Q.push t v
  let recv ~pool t = Q.await_pop ~pool t
end)

module Bench_mpmc_retry = Bench (struct
  module Q = Mpmc_queue
  type 'a t = 'a Q.t
  let make () = Q.make ()
  let send t v = Q.push t v
  let rec recv ~pool t =
    match Q.pop t with
    | Some v -> v
    | None ->
        Domain.cpu_relax () ;
        recv ~pool t
end)

module Bench_chan = Bench (struct
  module C = Domainslib.Chan
  type 'a t = 'a C.t
  let make () = C.make_unbounded ()
  let send t v = C.send t v
  let recv ~pool:_ t = C.recv t
end)

let () =
  Format.printf "@." ;
  Format.printf "num_domains: " ;
  for i = 1 to 8 do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  Format.printf "  Mpmc_pool: " ;
  Bench_mpmc_pool.run () ;
  Format.printf " Mpmc_retry: " ;
  Bench_mpmc_retry.run () ;
  Format.printf "       Chan: " ;
  Bench_chan.run ()
