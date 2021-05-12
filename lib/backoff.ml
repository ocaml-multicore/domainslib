type t =
  {
    mutable curr_backoff: int;
    max_backoff: int;
  }

let k = Domain.DLS.new_key Random.State.make_self_init

let create ?(curr_backoff=1) ?(max_backoff = 64) () =
  { curr_backoff; max_backoff }

let once state =
  let t = Random.State.int (Domain.DLS.get k) state.curr_backoff in
  state.curr_backoff <- min (2 * state.curr_backoff) state.max_backoff;
  if t = 0 then ()
  else begin
    (* expect cpu_relax to be O(10-100ns) *)
    for _ = 1 to 16 * t do
      Domain.Sync.cpu_relax ()
    done
  end

let reset state = state.curr_backoff <- 1