(* Test gets stuck if [Task.yield] is missing. *)

module T = Domainslib.Task

module Cell : sig

  type 'a t
  val make : T.pool -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a

end = struct

  type 'a t = { pool : T.pool ; cell : 'a option Atomic.t }

  let make pool = { pool ; cell = Atomic.make None }

  let rec push t x =
    if not (Atomic.compare_and_set t.cell None (Some x))
    then begin
      T.yield t.pool ;
      push t x
    end

  let rec pop t =
    match Atomic.get t.cell with
    | (Some x) as old when Atomic.compare_and_set t.cell old None -> x
    | _ ->
        T.yield t.pool ; (* try commenting *)
        pop t
end

let test pool () =
  let t = Cell.make pool in
  T.parallel_for pool ~start:1 ~finish:100 ~body:(fun i ->
    let p = T.async pool (fun () -> Cell.push t i) in
    let _ = Cell.pop t in
    T.await pool p
  )

let () =
  let pool = T.setup_pool ~num_domains:6 () in
  T.run pool (test pool) ;
  T.teardown_pool pool
