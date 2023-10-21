
let parallel_iter (f:'a->unit) arr cur_pool = 
  let arr_len = Array.length arr in
  Task.parallel_for cur_pool ~start:0 ~finish:(arr_len - 1) ~body:(fun i->
    f arr.(i))

let parallel_iteri (f:int->'a->unit) arr cur_pool =
  let arr_len = Array.length arr in
  Task.parallel_for cur_pool ~start:0 ~finish:(arr_len - 1) ~body:(fun i->
    f i arr.(i))
