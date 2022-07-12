
(** Pool has only one domain, do nothing *)
open Effect
open Effect.Deep

type pool = {name: string option; mutable active: bool}

type 'a promise_state =
  Returned of 'a
| Raised of exn * Printexc.raw_backtrace
| Pending of (('a, unit) continuation) list

type 'a promise = 'a promise_state ref

type 'a task = unit -> 'a

type _ t += Wait : 'a promise -> 'a t

let named_pools = Hashtbl.create 8

let get_pool_data pool =
  match pool.active with
  | true -> true
  | false -> raise (Invalid_argument "pool already torn down")

let setup_pool ?name ~num_additional_domains () =
  if num_additional_domains < 0 then
   raise (Invalid_argument
   "Task.setup_pool: num_additional_domains must be at least 0")
  else
    let p = {name = name; active = true } in
    begin match name with
    | None -> ()
    | Some x -> Hashtbl.add named_pools x p
  end;
    p

let teardown_pool pool =
  pool.active <- false;
  match pool.name with
  | None -> ()
  | Some x -> Hashtbl.remove named_pools x

let lookup_pool name = Hashtbl.find_opt named_pools name

let get_num_domains _ = 1

let do_task f promise =
  let result =
    try
      let v = f () in
      Returned v
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      Raised (e, bt)
    in
    promise := result

let async pool f =
  assert (get_pool_data pool);
  let p = ref (Pending []) in
  do_task f p;
  p

let await pool promise =
  assert (get_pool_data pool);
  match !promise with
  | Returned v -> v
  | Raised (e, bt) -> Printexc.raise_with_backtrace e bt
  | Pending _ -> perform (Wait promise)

let run pool f =
  assert (get_pool_data pool);
  f ()

let parallel_for_reduce ?(chunk_size=0) ~start ~finish ~body pool reduce_fun init =
  assert (get_pool_data pool);
  ignore chunk_size;
  let prev = ref init in
  for i = start to finish do
    prev := reduce_fun !prev (body i)
  done;
  !prev

let parallel_for ?(chunk_size=0) ~start ~finish ~body pool =
  assert (get_pool_data pool);
  ignore chunk_size;
  for i = start to finish do
    body i
  done


let parallel_scan pool op elements =
  assert (get_pool_data pool);
  let result = Array.copy elements in
  for i = 1 to (Array.length elements - 1) do
    result.(i) <- op result.(i - 1) elements.(i)
  done;
  result
