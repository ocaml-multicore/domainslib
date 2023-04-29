open Effect
open Effect.Deep

type 'a task = unit -> 'a

type message =
| Work of (unit -> unit)
  (* Invariant: the Work function does not need to run under the 'step' handler,
     it installs its own handler or re-invokes a deep-handler continuation. *)
| Quit

type task_chan = message Multi_channel.t

type pool_data = {
  domains : unit Domain.t array;
  task_chan : task_chan;
  name: string option
}

type pool = pool_data option Atomic.t

type 'a promise_state =
  Returned of 'a
| Raised of exn * Printexc.raw_backtrace
| Pending of (('a, unit) continuation * task_chan) list

type 'a promise = 'a promise_state Atomic.t

type _ t += Wait : 'a promise * task_chan -> 'a t

let get_pool_data p =
  match Atomic.get p with
  | None -> invalid_arg "pool already torn down"
  | Some p -> p

let cont v (k, c) = Multi_channel.send c (Work (fun _ -> continue k v))
let discont e bt (k, c) = Multi_channel.send c (Work (fun _ ->
  discontinue_with_backtrace k e bt))

let do_task (type a) (f : unit -> a) (p : a promise) : unit =
  let action, result =
    try
      let v = f () in
      cont v, Returned v
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      discont e bt, Raised (e, bt)
  in
  match Atomic.exchange p result with
  | Pending l -> List.iter action l
  |  _ -> failwith "Task.do_task: impossible, can only set result of task once"

let await pool promise =
  let pd = get_pool_data pool in
  match Atomic.get promise with
  | Returned v -> v
  | Raised (e, bt) -> Printexc.raise_with_backtrace e bt
  | Pending _ -> perform (Wait (promise, pd.task_chan))

let step (type a) (f : a -> unit) (v : a) : unit =
  try_with f v
  { effc = fun (type a) (e : a t) ->
      match e with
      | Wait (p,c) -> Some (fun (k : (a, _) continuation) ->
          let rec loop () =
            let old = Atomic.get p in
            match old with
            | Pending l ->
                if Atomic.compare_and_set p old (Pending ((k,c)::l)) then ()
                else (Domain.cpu_relax (); loop ())
            | Returned v -> continue k v
            | Raised (e,bt) -> discontinue_with_backtrace k e bt
          in
          loop ())
      | _ -> None }

let async pool f =
  let pd = get_pool_data pool in
  let p = Atomic.make (Pending []) in
  Multi_channel.send pd.task_chan (Work (fun _ -> step (do_task f) p));
  p

let prepare_for_await chan () =
  let promise = Atomic.make (Pending []) in
  let release () =
    match Atomic.get promise with
    | (Returned _ | Raised _) -> ()
    | Pending _ ->
      match Atomic.exchange promise (Returned ()) with
      | Pending ks ->
        ks
        |> List.iter @@ fun (k, c) ->
           Multi_channel.send_foreign c (Work (fun _ -> continue k ()))
      | _ -> ()
  and await () =
    match Atomic.get promise with
    | (Returned _ | Raised _) -> ()
    | Pending _ -> perform (Wait (promise, chan))
  in
  Domain_local_await.{ release; await }

let rec worker task_chan =
  match Multi_channel.recv task_chan with
  | Quit -> Multi_channel.clear_local_state task_chan
  | Work f -> f (); worker task_chan

let worker task_chan =
  Domain_local_await.using
    ~prepare_for_await:(prepare_for_await task_chan)
    ~while_running:(fun () -> worker task_chan)

let run (type a) pool (f : unit -> a) : a =
  let pd = get_pool_data pool in
  let p = Atomic.make (Pending []) in
  step (fun _ -> do_task f p) ();
  let rec loop () : a =
    match Atomic.get p with
    | Pending _ ->
        begin
          try
            match Multi_channel.recv_poll pd.task_chan with
            | Work f -> f ()
            | Quit -> failwith "Task.run: tasks are active on pool"
          with Exit -> Domain.cpu_relax ()
        end;
        loop ()
   | Returned v -> v
   | Raised (e, bt) -> Printexc.raise_with_backtrace e bt
  in
  loop ()

let run pool f =
  Domain_local_await.using
    ~prepare_for_await:(prepare_for_await (get_pool_data pool).task_chan)
    ~while_running:(fun () -> run pool f)

let named_pools = Hashtbl.create 8
let named_pools_mutex = Mutex.create ()

let setup_pool ?name ~num_domains () =
  if num_domains < 0 then
    invalid_arg "Task.setup_pool: num_domains must be at least 0"
  else
  let task_chan = Multi_channel.make (num_domains+1) in
  let domains = Array.init num_domains (fun _ ->
    Domain.spawn (fun _ -> worker task_chan))
  in
  let p = Atomic.make (Some {domains; task_chan; name}) in
  begin match name with
    | None -> ()
    | Some x ->
        Mutex.lock named_pools_mutex;
        Hashtbl.add named_pools x p;
        Mutex.unlock named_pools_mutex
  end;
  p

let teardown_pool pool =
  let pd = get_pool_data pool in
  for _i=1 to Array.length pd.domains do
    Multi_channel.send pd.task_chan Quit
  done;
  Multi_channel.clear_local_state pd.task_chan;
  Array.iter Domain.join pd.domains;
  (* Remove the pool from the table *)
  begin match pd.name with
  | None -> ()
  | Some n ->
      Mutex.lock named_pools_mutex;
      Hashtbl.remove named_pools n;
      Mutex.unlock named_pools_mutex
  end;
  Atomic.set pool None

let lookup_pool name =
  Mutex.lock named_pools_mutex;
  let p = Hashtbl.find_opt named_pools name in
  Mutex.unlock named_pools_mutex;
  p

let get_num_domains pool =
  let pd = get_pool_data pool in
  Array.length pd.domains + 1

let parallel_for_reduce ?(chunk_size=0) ~start ~finish ~body pool reduce_fun init =
  let pd = get_pool_data pool in
  let chunk_size = if chunk_size > 0 then chunk_size
      else begin
        let n_domains = (Array.length pd.domains) + 1 in
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
  if finish < start
  then init
  else reduce_fun init (work start finish)

let parallel_for ?(chunk_size=0) ~start ~finish ~body pool =
  let pd = get_pool_data pool in
  let chunk_size = if chunk_size > 0 then chunk_size
      else begin
        let n_domains = (Array.length pd.domains) + 1 in
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
  let pd = get_pool_data pool in
  let n = Array.length elements in
  let p = min (n - 1) ((Array.length pd.domains) + 1) in
  let prefix_s = Array.copy elements in
  let scan_part op elements prefix_sum start finish =
    assert (Array.length elements > (finish - start));
    for i = (start + 1) to finish do
      prefix_sum.(i) <- op prefix_sum.(i - 1) elements.(i)
    done
  in
  if p < 2 then begin
    (* Do a sequential scan when number of domains or array's length is less
    than 2 *)
    scan_part op elements prefix_s 0 (n - 1);
    prefix_s
  end
  else begin
  let add_offset op prefix_sum offset start finish =
    assert (Array.length prefix_sum > (finish - start));
    for i = start to finish do
      prefix_sum.(i) <- op offset prefix_sum.(i)
    done
  in

  parallel_for pool ~chunk_size:1 ~start:0 ~finish:(p - 1)
  ~body:(fun i ->
    let s = (i * n) / (p ) in
    let e = (i + 1) * n / (p ) - 1 in
    scan_part op elements prefix_s s e);

  let x = ref prefix_s.(n/p - 1) in
  for i = 2 to p do
      let ind = i * n / p - 1 in
      x := op prefix_s.(ind) !x;
      prefix_s.(ind) <- !x
  done;

  parallel_for pool ~chunk_size:1 ~start:1 ~finish:(p - 1)
  ~body:( fun i ->
    let s = i * n / (p) in
    let e = (i + 1) * n / (p) - 2 in
    let offset = prefix_s.(s - 1) in
      add_offset op prefix_s offset s e
    );

  prefix_s
  end
