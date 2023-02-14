module T = Domainslib.Task

let max_children = 3
let size = 10
let elems = Array.init size (fun i -> i, "key" ^ string_of_int i)
let t = 
  let t = Batch_para_btree.create ~max_children () in
  Array.iter (fun (k,v )-> Batch_para_btree.insert t k v) elems;
  t

let test_correctness pool =
  let a = T.run pool (fun () -> 
      Format.printf "%a" (Batch_para_btree.pp Format.pp_print_string) t;
      Batch_para_btree.par_flatten t pool) in
  Array.iter (fun (k,_) -> Format.printf "%d; " k) a

let () = 
  let pool = T.setup_pool ~num_domains:0 () in
  test_correctness pool;
  T.teardown_pool pool
