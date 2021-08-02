type 'a task = unit -> 'a

type 'a promise = ('a, exn) result option Atomic.t

exception TasksActive

type task_msg =
  Task : 'a task * 'a promise -> task_msg
| Quit : task_msg

type pool =
  {domains : unit Domain.t array;
   task_chan : task_msg Multi_channel.t}

let do_task f p =
  try
    let res = f () in
    Atomic.set p (Some (Ok res))
  with e ->
    Atomic.set p (Some (Error e));
    match e with
    | TasksActive -> raise e
    | _ -> ()

let setup_pool ~num_additional_domains =
  let task_chan = Multi_channel.make (num_additional_domains+1) in
  let rec worker () =
    match Multi_channel.recv task_chan with
    | Quit -> Multi_channel.clear_local_state ();
    | Task (t, p) ->
        do_task t p;
        worker ()
  in
  let domains = Array.init num_additional_domains (fun _ -> Domain.spawn worker) in
  {domains; task_chan}

let async pool task =
  let p = Atomic.make None in
  Multi_channel.send pool.task_chan (Task(task,p));
  p

let rec await pool promise =
  match Atomic.get promise with
  | None ->
      begin
        try
          match Multi_channel.recv_poll pool.task_chan with
          | Task (t, p) -> do_task t p
          | Quit -> raise TasksActive
        with
        | Exit -> Domain.Sync.cpu_relax ()
      end;
      await pool promise
  | Some (Ok v) -> v
  | Some (Error e) -> raise e

let teardown_pool pool =
  for _i=1 to Array.length pool.domains do
    Multi_channel.send pool.task_chan Quit
  done;
  Multi_channel.clear_local_state ();
  Array.iter Domain.join pool.domains

let parallel_for_reduce ?(chunk_size=0) ~start ~finish ~body pool reduce_fun init =
  let chunk_size = if chunk_size > 0 then chunk_size
      else begin
        let n_domains = (Array.length pool.domains) + 1 in
        let n_tasks = finish - start + 1 in
        if n_domains = 1 then n_tasks
        else max 1 (n_tasks/(8*n_domains))
      end
  in
  let rec work s e =
    if e - s < chunk_size then
      let rec loop i acc =
        if i > e then acc
        else loop (i+1) (reduce_fun acc (body i))
      in
      loop (s+1) (body s)
    else begin
      let d = s + ((e - s) / 2) in
      let p = async pool (fun _ -> work s d) in
      let right = work (d+1) e in
      let left = await pool p in
      reduce_fun left right
    end
  in
  reduce_fun init (work start finish)

let parallel_for ?(chunk_size=0) ~start ~finish ~body pool =
  let chunk_size = if chunk_size > 0 then chunk_size
      else begin
        let n_domains = (Array.length pool.domains) + 1 in
        let n_tasks = finish - start + 1 in
        if n_domains = 1 then n_tasks
        else max 1 (n_tasks/(8*n_domains))
      end
  in
  let rec work pool fn s e =
    if e - s < chunk_size then
      for i = s to e do fn i done
    else begin
      let d = s + ((e - s) / 2) in
      let left = async pool (fun _ -> work pool fn s d) in
      work pool fn (d+1) e;
      await pool left
    end
  in
  work pool body start finish

let parallel_scan pool op elements =

  let n = Array.length elements in
  let p = (Array.length pool.domains) + 1 in
  let prefix_s = Array.copy elements in

  let scan_part op elements prefix_sum start finish =
    assert (Array.length elements >= (finish - start));
    for i = (start + 1) to finish do
      prefix_sum.(i) <- op prefix_sum.(i - 1) elements.(i)
    done
  in

  if (n <= p) || (p = 1) then begin
    (* Performs a sequential scan when number of elements is less than or equal
    to the pool size or if the number of domains is one *)
    scan_part op elements prefix_s 0 (n - 1);
    prefix_s
  end
  else begin

  let add_offset op prefix_sum offset start finish =
    assert (Array.length prefix_sum >= (finish - start));
    for i = start to finish do
      prefix_sum.(i) <- op offset prefix_sum.(i)
    done
  in

  parallel_for pool ~chunk_size:1 ~start:0 ~finish:(p - 1)
  ~body:(fun i ->
    let s = (i * n) / (p ) in
    let e = (i + 1) * n / (p ) - 1 in
    scan_part op elements prefix_s s e);

  if (p > 2) then begin
  let x = ref prefix_s.(n/p - 1) in
  for i = 2 to p do
      let ind = i * n / p - 1 in
      x := op prefix_s.(ind) !x;
      prefix_s.(ind) <- !x
  done
  end;

  parallel_for pool ~chunk_size:1 ~start:1 ~finish:(p - 1)
  ~body:( fun i ->
    let s = i * n / (p) in
    let e = (i + 1) * n / (p) - 2 in
    let offset = prefix_s.(s - 1) in
      add_offset op prefix_s offset s e
    );
  prefix_s
  end

