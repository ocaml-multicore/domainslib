type 'a task = unit -> 'a

type ('a, 'b) reducer = {
  init : 'a;
  reduce : 'a -> 'b -> 'a;
  num_tasks: int Atomic.t; (* Number of active tasks *)
  result_chan : 'b Chan.t;
  reduced : bool Atomic.t
}

type task_msg =
  Task : {task : 'b task; reducer : ('a,'b) reducer} -> task_msg
| Quit : task_msg

type pool =
  {domains : unit Domain.t array;
   task_chan : task_msg Chan.t}

let do_task f r =
  try
    let res = f () in
    Chan.send r.result_chan res;
    Atomic.decr r.num_tasks
  with e ->
    print_endline (Printexc.to_string e);
    exit 1

let make_reducer reduce init =
  {init; reduce;
   num_tasks = Atomic.make 0;
   result_chan = Chan.make_unbounded ();
   reduced = Atomic.make false}

let setup_pool ~num_domains =
  let task_chan = Chan.make_unbounded () in
  let rec worker () =
    match Chan.recv task_chan with
    | Quit -> ()
    | Task {task; reducer} ->
        do_task task reducer;
        worker ()
  in
  let domains = Array.init num_domains (fun _ -> Domain.spawn worker) in
  {domains; task_chan}

let add_task pool reducer task =
  Atomic.incr reducer.num_tasks;
  Chan.send pool.task_chan (Task {task; reducer})

let rec reduce pool reducer =
  if Atomic.get reducer.num_tasks = 0 then begin
    if not (Atomic.compare_and_set reducer.reduced false true) then
      raise (Invalid_argument "Domainslib.Task_pool.reduce: already reduced");
    let rec loop acc =
      match Chan.recv_poll reducer.result_chan with
      | None -> acc
      | Some v -> loop (reducer.reduce acc v)
    in
    loop reducer.init
  end else begin
    begin match Chan.recv_poll pool.task_chan with
    | None -> Domain.Sync.cpu_relax ()
    | Some (Task {task; reducer}) -> do_task task reducer
    | Some Quit -> failwith "Domainslib.Task_pool.reduce: unexpectedly quitting"
    end;
    reduce pool reducer
  end

let teardown_pool pool =
  for _i=1 to Array.length pool.domains do
    Chan.send pool.task_chan Quit
  done;
  Array.iter Domain.join pool.domains

let parallel_for_reduce pool reduce_fun init ~chunk_size ~start ~finish ~body =
  assert (chunk_size > 0);
  let r = make_reducer reduce_fun init in
  let work s e =
    let rec loop i acc =
      if i > e then acc
      else loop (i+1) (reduce_fun acc (body i))
    in
    loop s init
  in
  let rec loop i =
    if i+chunk_size > finish then
      add_task pool r (fun _ -> work i finish)
    else begin
      add_task pool r (fun _ -> work i (i+chunk_size-1));
      loop (i+chunk_size)
    end
  in
  loop start;
  reduce pool r

let parallel_for pool ~chunk_size ~start ~finish ~body =
  parallel_for_reduce pool (fun _ _ -> ()) () ~chunk_size ~start ~finish ~body
