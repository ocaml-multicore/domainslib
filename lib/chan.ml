(* mutex_condvar will be used per domain; so multiple fibers or
   systhreads may share a mutex_condvar variable *)
type mutex_condvar = {
  mutex: Mutex.t;
  condition: Condition.t
}

type waiting_notified =
  | Waiting
  | Notified

type 'a contents =
  | Empty of {receivers: ('a option ref * mutex_condvar) Fun_queue.t}
  | NotEmpty of {senders: ('a * waiting_notified ref * mutex_condvar) Fun_queue.t; messages: 'a Fun_queue.t}

type 'a t = {
  buffer_size: int option;
  contents: 'a contents Atomic.t
}

let mutex_condvar_key =
  Domain.DLS.new_key (fun () ->
    let m = Mutex.create () in
    let c = Condition.create () in
    {mutex=m; condition=c})

let make_bounded n =
  if n < 0 then raise (Invalid_argument "Chan.make_bounded") ;
  {buffer_size= Some n;
   contents = Atomic.make (Empty {receivers= Fun_queue.empty; })}

let make_unbounded () =
  {buffer_size= None;
   contents = Atomic.make (Empty {receivers= Fun_queue.empty})}

(* [send'] is shared by both the blocking and polling versions. Returns a
 * boolean indicating whether the send was successful. Hence, it always returns
 * [true] if [polling] is [false]. *)
let send' {buffer_size; contents} v ~polling =
  let open Fun_queue in
  let rec loop backoff =
    let old_contents = Atomic.get contents in
    match old_contents with
    | Empty {receivers} -> begin
      (* The channel is empty (no senders) *)
      match pop receivers with
      | None ->
          (* The channel is empty (no senders) and no waiting receivers *)
          if buffer_size = Some 0 then
            (* The channel is empty (no senders), no waiting receivers, and
              * buffer size is 0 *)
            begin if not polling then begin
              (* The channel is empty (no senders), no waiting receivers,
                * buffer size is 0 and we're not polling *)
              let mc = Domain.DLS.get mutex_condvar_key in
              let cond_slot = ref Waiting in
              let new_contents =
                NotEmpty
                  {messages= empty; senders= push empty (v, cond_slot, mc)}
              in
              if Atomic.compare_and_set contents old_contents new_contents
              then begin
                Mutex.lock mc.mutex;
                while !cond_slot = Waiting do
                  Condition.wait mc.condition mc.mutex
                done;
                Mutex.unlock mc.mutex;
                true
              end else begin
                match backoff with
                | Some b -> (Backoff.once b; loop backoff)
                | None -> (let b = Backoff.create () in
                       Backoff.once b; loop (Some b))
              end
            end else
              (* The channel is empty (no senders), no waiting receivers,
                * buffer size is 0 and we're polling *)
              false
            end
          else
            (* The channel is empty (no senders), no waiting receivers, and
              * the buffer size is non-zero *)
            let new_contents =
              NotEmpty {messages= push empty v; senders= empty}
            in
            if Atomic.compare_and_set contents old_contents new_contents
            then true
            else begin
              match backoff with
              | Some b -> (Backoff.once b; loop backoff)
              | None -> (let b = Backoff.create () in
                     Backoff.once b; loop (Some b))
            end
      | Some ((r, mc), receivers') ->
          (* The channel is empty (no senders) and there are waiting receivers
           * *)
          let new_contents = Empty {receivers= receivers'} in
          if Atomic.compare_and_set contents old_contents new_contents
          then begin
            r := Some v;
            Mutex.lock mc.mutex;
            Mutex.unlock mc.mutex;
            Condition.broadcast mc.condition;
            true
          end else begin
            match backoff with
            | Some b -> (Backoff.once b; loop backoff)
            | None -> (let b = Backoff.create () in
                   Backoff.once b; loop (Some b))
          end
    end
    | NotEmpty {senders; messages} ->
        (* The channel is not empty *)
        if buffer_size = Some (length messages) then
          (* The channel is not empty, and the buffer is full *)
          begin if not polling then
            (* The channel is not empty, the buffer is full and we're not
              * polling *)
            let cond_slot = ref Waiting in
            let mc = Domain.DLS.get mutex_condvar_key in
            let new_contents =
              NotEmpty {senders= push senders (v, cond_slot, mc); messages}
            in
            if Atomic.compare_and_set contents old_contents new_contents then begin
              Mutex.lock mc.mutex;
              while !cond_slot = Waiting do
                Condition.wait mc.condition mc.mutex;
              done;
              Mutex.unlock mc.mutex;
              true
            end else begin
              match backoff with
              | Some b -> (Backoff.once b; loop backoff)
              | None -> (let b = Backoff.create () in
                     Backoff.once b; loop (Some b))
            end
          else
            (* The channel is not empty, the buffer is full and we're
              * polling *)
            false
          end
        else
          (* The channel is not empty, and the buffer is not full *)
          let new_contents =
            NotEmpty {messages= push messages v; senders}
          in
          if Atomic.compare_and_set contents old_contents new_contents
          then true
          else begin
            match backoff with
            | Some b -> (Backoff.once b; loop backoff)
            | None -> (let b = Backoff.create () in
                   Backoff.once b; loop (Some b))
          end
  in
  loop None

let send c v =
  let r = send' c v ~polling:false in
  assert r

let send_poll c v = send' c v ~polling:true

(* [recv'] is shared by both the blocking and polling versions. Returns a an
 * optional value indicating whether the receive was successful. Hence, it
 * always returns [Some v] if [polling] is [false]. *)
let recv' {buffer_size; contents} ~polling =
  let open Fun_queue in
  let rec loop backoff =
    let old_contents = Atomic.get contents in
    match old_contents with
    | Empty {receivers} ->
        (* The channel is empty (no senders) *)
        if not polling then begin
          (* The channel is empty (no senders), and we're not polling *)
          let msg_slot = ref None in
          let mc = Domain.DLS.get mutex_condvar_key in
          let new_contents =
            Empty {receivers= push receivers (msg_slot, mc)}
          in
          if Atomic.compare_and_set contents old_contents new_contents then
          begin
            Mutex.lock mc.mutex;
            while !msg_slot = None do
              Condition.wait mc.condition mc.mutex;
            done;
            Mutex.unlock mc.mutex;
            !msg_slot
          end else begin
            match backoff with
            | Some b -> (Backoff.once b; loop backoff)
            | None -> (let b = Backoff.create () in
                   Backoff.once b; loop (Some b))
          end
        end else
          (* The channel is empty (no senders), and we're polling *)
          None
    | NotEmpty {senders; messages} ->
        (* The channel is not empty *)
        match (pop messages, pop senders) with
        | None, None ->
            (* The channel is not empty, but no senders or messages *)
            failwith "Chan.recv: Impossible - channel state"
        | Some (m, messages'), None ->
            (* The channel is not empty, there is a message and no
              * waiting senders *)
            let new_contents =
              if length messages' = 0 then
                Empty {receivers = empty}
              else
                NotEmpty {messages= messages'; senders}
            in
            if Atomic.compare_and_set contents old_contents new_contents
            then Some m
            else begin
              match backoff with
              | Some b -> (Backoff.once b; loop backoff)
              | None -> (let b = Backoff.create () in
                     Backoff.once b; loop (Some b))
            end
        | None, Some ((m, c, mc), senders') ->
            (* The channel is not empty, there are no messages, and there
              * is a waiting sender. This is only possible is the buffer
              * size is 0. *)
            assert (buffer_size = Some 0) ;
            let new_contents =
              if length senders' = 0 then
                Empty {receivers = empty}
              else
                NotEmpty {messages; senders= senders'}
            in
            if Atomic.compare_and_set contents old_contents new_contents
            then begin
              c := Notified;
              Mutex.lock mc.mutex;
              Mutex.unlock mc.mutex;
              Condition.broadcast mc.condition;
              Some m
            end else begin
              match backoff with
              | Some b -> (Backoff.once b; loop backoff)
              | None -> (let b = Backoff.create () in
                     Backoff.once b; loop (Some b))
            end
        | Some (m, messages'), Some ((ms, sc, mc), senders') ->
            (* The channel is not empty, there is a message, and there is a
              * waiting sender. *)
            let new_contents =
              NotEmpty {messages= push messages' ms; senders= senders'}
            in
            if Atomic.compare_and_set contents old_contents new_contents
            then begin
              sc := Notified;
              Mutex.lock mc.mutex;
              Mutex.unlock mc.mutex;
              Condition.broadcast mc.condition;
              Some m
            end else begin
              match backoff with
              | Some b -> (Backoff.once b; loop backoff)
              | None -> (let b = Backoff.create () in
                     Backoff.once b; loop (Some b))
            end
  in
  loop None

let recv c =
  match recv' c ~polling:false with
  | None -> failwith "Chan.recv: impossible - no message"
  | Some m -> m

let recv_poll c =
  match Atomic.get c.contents with
  | Empty _ -> None
  | _ -> recv' c ~polling:true
