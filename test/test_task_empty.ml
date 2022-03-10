open Domainslib

let array_size = 0

let pool = Task.setup_pool ~num_additional_domains:0 ()
let res = Task.run pool (fun () ->
    Task.parallel_for_reduce ~chunk_size:0 ~start:0 ~finish:(array_size-1) ~body:(fun _ -> 1) pool (+) 0);;
Task.teardown_pool pool;;
assert(res = array_size)
