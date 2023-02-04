module T = Domainslib.Task

let max_keys = 4
let size = 10_000_000
let elems = Array.init size (fun i -> i, "key" ^ string_of_int i)
let pool = T.setup_pool ~num_domains:(Domain.recommended_domain_count () - 1) ()
let t = T.run pool (fun () -> Batch_para_btree.build ~max_keys pool elems)
let test_correctness () =
  Array.for_all (fun (k,v) -> 
      match Btree.search t k with
      | Some v' -> v' = v
      | None -> false ) elems

let () = 
  let res = test_correctness () in
  T.teardown_pool pool;
  assert(res)