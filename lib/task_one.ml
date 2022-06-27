
(** Pool has only one domain, do nothing *)

type pool = unit

type 'a promise_state =
  Returned of 'a
| Raised of exn * Printexc.raw_backtrace
| Pending of (('a, unit) continuation * task_chan) list

type 'a promise = 'a promise_state Atomic.t

let setup_pool ?name num_additional_domains () = ignore num_additional_domains

let teardown_pool pool = pool

let lookup_pool name = ignore name

let get_num_domains _ = 1

let do_task f p =