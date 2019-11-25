type 'a contents =
  | Empty of {receivers: ('a option ref * Domain.id) Fun_queue.t}
  | NotEmpty of {senders: ('a * Domain.id) Fun_queue.t; messages: 'a Fun_queue.t}

type 'a t = {buffer_size: int; contents: 'a contents Atomic.t}

let make n =
  if n < 0 then raise (Invalid_argument "Chan.make") ;
  {buffer_size= n; contents = Atomic.make (Empty {receivers= Fun_queue.empty})}

let rec send {buffer_size; contents} v =
  let open Fun_queue in
  let wakeup = ref None in
  let success =
    Domain.Sync.critical_section (fun () ->
        let old_contents = Atomic.get contents in
        match old_contents with
        | Empty {receivers} -> (
          (* The channel is empty *)
          match pop receivers with
          | None ->
              (* The channel is empty and no waiting receivers *)
              if buffer_size = 0 then
                (* The channel is empty, no waiting receivers, and buffer size
                 * is 0 *)
                let new_contents =
                  NotEmpty
                    {messages= empty; senders= push empty (v, Domain.self ())}
                in
                if Atomic.compare_and_set contents old_contents new_contents
                then ( Domain.Sync.wait () ; true )
                else false
              else
                (* The channel is empty, no waiting receivers, and the buffer
                 * size is non-zero *)
                let new_contents =
                  NotEmpty {messages= push empty v; senders= empty}
                in
                Atomic.compare_and_set contents old_contents new_contents
          | Some ((r, d), receivers') ->
              (* The channel is empty and there are waiting receivers *)
              wakeup := Some (r, d) ;
              let new_contents = Empty {receivers= receivers'} in
              Atomic.compare_and_set contents old_contents new_contents )
        | NotEmpty {senders; messages} ->
            (* The channel is not empty *)
            if length messages = buffer_size then
              (* The channel is not empty, and the buffer is full *)
              let new_contents =
                NotEmpty {senders= push senders (v, Domain.self ()); messages}
              in
              if Atomic.compare_and_set contents old_contents new_contents then
                ( Domain.Sync.wait () ; true )
              else false
            else
              (* The channel is not empty, and the buffer is not full *)
              let new_contents =
                NotEmpty {messages= push messages v; senders}
              in
              Atomic.compare_and_set contents old_contents new_contents)
  in
  if success then (
    match !wakeup with
    | None ->
        ()
    | Some (r, d) ->
        r := Some v ;
        Domain.Sync.notify d )
  else send {buffer_size; contents} v

let rec recv {buffer_size; contents} =
  let open Fun_queue in
  let msg_slot = ref None in
  let wakeup = ref None in
  let success =
    Domain.Sync.critical_section (fun () ->
        let old_contents = Atomic.get contents in
        match old_contents with
        | Empty {receivers} ->
            (* The channel is empty *)
            let new_contents =
              Empty {receivers= push receivers (msg_slot, Domain.self ())}
            in
            if Atomic.compare_and_set contents old_contents new_contents then
              (Domain.Sync.wait (); true)
            else false
        | NotEmpty {senders; messages} ->
            (* The channel is not empty *)
            let new_contents =
              match (pop messages, pop senders) with
              | None, None ->
                  failwith "Chan.recv: Impossible - channel state"
              | Some (m, messages'), None ->
                  (* The channel is not empty, there is a message and no
                   * waiting senders *)
                  msg_slot := Some m ;
                  if length messages' = 0 then
                    Empty {receivers = empty}
                  else
                    NotEmpty {messages= messages'; senders}
              | None, Some ((m, s), senders') ->
                  (* The channel is not empty, there are no messages, and there
                   * is a waiting sender. This is only possible is the buffer
                   * size is 0. *)
                  assert (buffer_size = 0) ;
                  msg_slot := Some m ;
                  wakeup := Some s ;
                  if length senders' = 0 then
                    Empty {receivers = empty}
                  else
                    NotEmpty {messages; senders= senders'}
              | Some (m, messages'), Some ((ms, s), senders') ->
                  (* The channel is not empty, there is a message, and there is a
                   * waiting sender. *)
                  msg_slot := Some m ;
                  wakeup := Some s ;
                  NotEmpty {messages= push messages' ms; senders= senders'}
            in
            Atomic.compare_and_set contents old_contents new_contents)
  in
  if success then (
    (match !wakeup with None -> () | Some s -> Domain.Sync.notify s) ;
    match !msg_slot with
    | None ->
        failwith "Chan.recv: impossible - no message"
    | Some m ->
        m )
  else recv {buffer_size; contents}
