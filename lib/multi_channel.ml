

type 'a t = {
  push_index: int Atomic.t;
  take_index: int Atomic.t;
  mask: int;
  channels: 'a Chan.t array;
}

let rec log2 n =
  if n <= 1 then 0 else 1 + (log2 (n asr 1))

let make n =
  let sz = Int.shift_left 1 ((log2 n)+1) in
  assert ((sz > n) && (sz > 0));
  assert (Int.logand sz (sz -1) == 0);
  { push_index = Atomic.make 0;
    take_index = Atomic.make 0;
    mask = sz - 1;
    channels = Array.init sz (fun _ -> Chan.make_unbounded ()) }

let send mchan v =
  let i = Atomic.fetch_and_add mchan.push_index 1 in
  (*Printf.printf "send: %d\n%!" i;*)
  Chan.send mchan.channels.(Int.logand i mchan.mask) v

let recv mchan =
  let i = Atomic.fetch_and_add mchan.take_index 1 in
  (*Printf.printf "recv: %d\n%!" i;*)
  Chan.recv mchan.channels.(Int.logand i mchan.mask)


(*
 THIS DOESN'T WORK, ANOTHER RECV CAN HOIST THE MESSAGE YOU WANTED
 *)
let recv_poll mchan =
  let rec loop () =
    let push_idx = Atomic.get mchan.push_index in
    let take_idx = Atomic.get mchan.take_index in
    (* FIXME: does wrapping behave as expected here? *)
    if take_idx - push_idx < 0 then
      if Atomic.compare_and_set mchan.take_index take_idx (take_idx+1) then begin
        (*Printf.printf "recv_poll: %d\n%!" take_idx;*)
        Some (Chan.recv mchan.channels.(Int.logand take_idx mchan.mask))
      end
      else loop ()
    else
      None
  in
  loop ()
