type 'a task = unit -> 'a

type 'a promise = ('a, exn) result option Atomic.t

exception TasksActive

type task_msg =
  Task : 'a task * 'a promise -> task_msg
| Quit : task_msg

type pool =
  {domains : unit Domain.t array;
   task_chan : task_msg Chan.t}

let do_task f p =
  try
    let res = f () in
    Atomic.set p (Some (Ok res))
  with e ->
    Atomic.set p (Some (Error e));
    match e with
    | TasksActive -> raise e
    | _ -> ()

let setup_pool ~num_domains =
  let task_chan = Chan.make_unbounded () in
  let rec worker () =
    match Chan.recv task_chan with
    | Quit -> ()
    | Task (t, p) ->
        do_task t p;
        worker ()
  in
  let domains = Array.init num_domains (fun _ -> Domain.spawn worker) in
  {domains; task_chan}

let async pool task =
  let p = Atomic.make None in
  Chan.send pool.task_chan (Task(task,p));
  p

let rec await pool promise =
  match Atomic.get promise with
  | None ->
      begin match Chan.recv_poll pool.task_chan with
      | None -> Domain.Sync.cpu_relax ()
      | Some (Task (t, p)) -> do_task t p
      | Some Quit -> raise TasksActive
      end;
      await pool promise
  | Some (Ok v) -> v
  | Some (Error e) -> raise e

let teardown_pool pool =
  for _i=1 to Array.length pool.domains do
    Chan.send pool.task_chan Quit
  done;
  Array.iter Domain.join pool.domains

let parallel_for_reduce pool reduce_fun init ~chunk_size ~start ~finish ~body =
  assert (chunk_size > 0);
  let work s e =
    let rec loop i acc =
      if i > e then acc
      else loop (i+1) (reduce_fun acc (body i))
    in
    loop s init
  in
  let rec loop i acc =
    if i+chunk_size > finish then
      let p = async pool (fun _ -> work i finish) in
      p::acc
    else begin
      let p = async pool (fun _ -> work i (i+chunk_size-1)) in
      loop (i+chunk_size) (p::acc)
    end
  in
  let ps = loop start [] in
  let results = List.map (await pool) ps in
  List.fold_left reduce_fun init results

let parallel_for pool ~chunk_size ~start ~finish ~body =
  assert (chunk_size > 0);
  let work s e =
    for i=s to e do
      body i
    done
  in
  let rec loop i acc =
    if i+chunk_size > finish then
      let p = async pool (fun _ -> work i finish) in
      p::acc
    else begin
      let p = async pool (fun _ -> work i (i+chunk_size-1)) in
      loop (i+chunk_size) (p::acc)
    end
  in
  let ps = loop start [] in
  List.iter (await pool) ps

let parallel_scan pool op elements =

  let scan_part op elements prefix_sum start finish =
    assert (Array.length elements > (finish - start));
    for i = (start + 1) to finish do
      prefix_sum.(i) <- op prefix_sum.(i - 1) elements.(i)
    done
  in
  let add_offset op prefix_sum offset start finish =
    assert (Array.length prefix_sum > (finish - start));
    for i = start to finish do
      prefix_sum.(i) <- op offset prefix_sum.(i)
    done
  in
  let n = Array.length elements in
  let p = (Array.length pool.domains) + 1 in
  let prefix_s = Array.copy elements in

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
