(*
 * Copyright (c) 2021, Tom Kelly <ctk21@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Ws_deque = Saturn.Work_stealing_deque.M

type mutex_condvar = {
  mutex: Mutex.t;
  condition: Condition.t
}

type waiting_status =
  | Waiting
  | Released

type dls_state = {
  mutable id: int;
  mutable steal_offsets: int array;
  rng_state: Random.State.t;
  mc: mutex_condvar;
}

module Foreign_queue = Saturn.Queue

type 'a t = {
  channels: 'a Ws_deque.t array;
  (* Queue for enqueuing work from outside of the pool. *)
  foreign_queue: 'a Foreign_queue.t;
  waiters: (waiting_status ref * mutex_condvar ) Chan.t;
  next_domain_id: int Atomic.t;
  recv_block_spins: int;
  dls_key: dls_state Domain.DLS.key;
}

let dls_make_key () =
  let open Stdlib in
  Domain.DLS.new_key (fun () ->
    {
      id = -1;
      steal_offsets = Array.make 1 0;
      rng_state = Random.State.make_self_init ();
      mc = {mutex=Mutex.create (); condition=Condition.create ()};
    })

let rec log2 n =
  if n <= 1 then 0 else 1 + (log2 (n asr 1))

let make ?(recv_block_spins = 2048) n =
  let open Stdlib in
  { channels = Array.init n (fun _ -> Ws_deque.create ());
    foreign_queue = Foreign_queue.create ();
    waiters = Chan.make_unbounded ();
    next_domain_id = Atomic.make 0;
    recv_block_spins;
    dls_key = dls_make_key ()
    }

let register_domain mchan =
  let id = Atomic.fetch_and_add mchan.next_domain_id 1 in
  let open Stdlib  in
  assert(id < Array.length mchan.channels);
  id

let init_domain_state mchan dls_state =
  let id = register_domain mchan in
  let open Stdlib  in
  let len = Array.length mchan.channels in
  dls_state.id <- id;
  dls_state.steal_offsets <- Array.init (len - 1) (fun i -> (id + i + 1) mod len);
  dls_state
  [@@inline never]

let get_local_state mchan =
  let dls_state = Domain.DLS.get mchan.dls_key in
  if dls_state.id >= 0 then begin
    let open Stdlib  in
    assert (dls_state.id < Array.length mchan.channels);
    dls_state
  end
  else (init_domain_state mchan dls_state)
  [@@inline]

let clear_local_state mchan =
  let dls_state = Domain.DLS.get mchan.dls_key in
  dls_state.id <- (-1)

let rec check_waiters mchan =
  match Chan.recv_poll mchan.waiters with
    | None -> ()
    | Some (status, mc) ->
      (* avoid the lock if we possibly can *)
      if !status = Released then check_waiters mchan
      else begin
        Mutex.lock mc.mutex;
        match !status with
        | Waiting ->
          begin
            status := Released;
            Mutex.unlock mc.mutex;
            Condition.broadcast mc.condition
          end
        | Released ->
          begin
            (* this waiter is already released, it might have found something on a poll *)
            Mutex.unlock mc.mutex;
            check_waiters mchan
          end
      end

let send_foreign mchan v =
  Foreign_queue.push mchan.foreign_queue v;
  check_waiters mchan

let send mchan v =
  let id = (get_local_state mchan).id in
  let open Stdlib  in
  Ws_deque.push (Array.unsafe_get mchan.channels id) v;
  check_waiters mchan

let rec recv_poll_loop mchan dls cur_offset =
  let offsets = dls.steal_offsets in
  let open Stdlib  in
  let k = (Array.length offsets) - cur_offset in
  if k = 0 then raise Exit
  else begin
    let idx = cur_offset + (Random.State.int dls.rng_state k) in
    let t = Array.unsafe_get offsets idx in
    let channel = Array.unsafe_get mchan.channels t in
    try
      Ws_deque.steal channel
    with
      | Exit ->
        begin
          Array.unsafe_set offsets idx (Array.unsafe_get offsets cur_offset);
          Array.unsafe_set offsets cur_offset t;
          recv_poll_loop mchan dls (cur_offset+1)
        end
  end

let recv_poll_with_dls mchan dls =
  try
    let open Stdlib  in
    Ws_deque.pop (Array.unsafe_get mchan.channels dls.id)
  with
    | Exit ->
      match Foreign_queue.pop_opt mchan.foreign_queue with
      | None -> recv_poll_loop mchan dls 0
      | Some v -> v
  [@@inline]

let recv_poll mchan =
  recv_poll_with_dls mchan (get_local_state mchan)

let rec recv_poll_repeated mchan dls repeats =
  try
    recv_poll_with_dls mchan dls
  with
    | Exit ->
      if repeats = 1 then raise Exit
      else begin
        Domain.cpu_relax ();
        recv_poll_repeated mchan dls (repeats - 1)
      end

let rec recv mchan =
  let dls = get_local_state mchan in
  try
    recv_poll_repeated mchan dls mchan.recv_block_spins
  with
    Exit ->
      begin
        (* Didn't find anything, prepare to block:
         *  - enqueue our wait block in the waiter queue
         *  - check the queue again
         *  - go to sleep if our wait block has not been notified
         *  - when notified retry the recieve
         *)
        let status = ref Waiting in
        let mc = dls.mc in
        Chan.send mchan.waiters (status, mc);
        try
          let v = recv_poll mchan in
          (* need to check the status as might take an item
            which is not the one an existing sender has woken us
            to take *)
          Mutex.lock mc.mutex;
          begin match !status with
          | Waiting -> (status := Released; Mutex.unlock mc.mutex)
          | Released ->
            (* we were simultaneously released from a sender;
              so need to release a waiter *)
            (Mutex.unlock mc.mutex; check_waiters mchan)
          end;
          v
        with
          | Exit ->
            if !status = Waiting then begin
               Mutex.lock mc.mutex;
               while !status = Waiting do
                 Condition.wait mc.condition mc.mutex
               done;
               Mutex.unlock mc.mutex
            end;
            recv mchan
      end
