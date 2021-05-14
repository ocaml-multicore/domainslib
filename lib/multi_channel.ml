

type mutex_condvar = {
  mutex: Mutex.t;
  condition: Condition.t
}

let mc_key =
  Domain.DLS.new_key (fun () ->
    let m = Mutex.create () in
    let c = Condition.create () in
    {mutex=m; condition=c})

type waiting_notified =
  | Waiting
  | Notified

type 'a t = {
  mask: int;
  channels: 'a Chan.t array;
  waiters: (waiting_notified ref * mutex_condvar ) Chan.t;
}

let rec log2 n =
  if n <= 1 then 0 else 1 + (log2 (n asr 1))

let make n =
  let sz = Int.shift_left 1 ((log2 n)+1) in
  assert ((sz > n) && (sz > 0));
  assert (Int.logand sz (sz -1) == 0);
  { mask = sz - 1;
    channels = Array.init sz (fun _ -> Chan.make_unbounded ());
    waiters = Chan.make_unbounded ()
    }

let check_waiters mchan =
  match Chan.recv_poll mchan.waiters with
    | None -> ()
    | Some (status, mc) ->
      begin
        status := Notified;
        Mutex.lock mc.mutex;
        Mutex.unlock mc.mutex;
        Condition.broadcast mc.condition;
      end

let send mchan v =
  let id = (Domain.self () :> int) in
  let res = Chan.send mchan.channels.(Int.logand id mchan.mask) v in
  check_waiters mchan;
  res

let rec recv_poll_loop mchan cur left =
  if left = 0 then None
  else begin
    match Chan.recv_poll mchan.channels.(Int.logand cur mchan.mask) with
      | Some _ as v -> v
      | None -> recv_poll_loop mchan (cur+1) (left-1)
  end

let recv_poll mchan =
  recv_poll_loop mchan (Domain.self () :> int) (Array.length mchan.channels)

let rec recv mchan =
  match recv_poll mchan with
    | Some v -> v
    | None ->
      begin
        (* Didn't find any thing, prepare to block:
         *  - enqueue our wait block in the waiter queue
         *  - check the queue again
         *  - go to sleep if our wait block has not been notified
         *  - when notified retry the recieve
         *)
        let status = ref Waiting in
        let mc = Domain.DLS.get mc_key in
        Chan.send mchan.waiters (status, mc);
        match recv_poll mchan with
          | Some v -> v
          | None ->
            begin
               Mutex.lock mc.mutex;
               while !status = Waiting do
                 Condition.wait mc.condition mc.mutex
               done;
               Mutex.unlock mc.mutex;
               recv mchan
            end
      end
