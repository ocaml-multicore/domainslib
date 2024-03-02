type 'a t = 'a array

(** {1 Iterators} *)

let iter f a p = 
  let n = Stdlib.Array.length a in 
  Task.parallel_for p ~start:0 ~finish:(n-1) ~body:(fun i -> f @@ Stdlib.Array.get a i)

let iteri f a p = 
  let n = Stdlib.Array.length a in 
  Task.parallel_for p ~start:0 ~finish:(n-1) ~body:(fun i -> f i @@ Stdlib.Array.get a i)

let map f a p =
  let n = Stdlib.Array.length a in 
  let res = Stdlib.Array.make n @@ f (Stdlib.Array.get a 0) in
  Task.parallel_for p ~start:0 ~finish:(n-1) ~body:(fun i -> Stdlib.Array.set res i @@ f (Stdlib.Array.get a i));
  res

let map_inplace f a p =
  let n = Stdlib.Array.length a in 
  Task.parallel_for p ~start:0 ~finish:(n-1) ~body:(fun i -> Stdlib.Array.set a i @@ f (Stdlib.Array.get a i))

let mapi f a p =
  let n = Stdlib.Array.length a in 
  let res = Stdlib.Array.make n @@ f 0 (Stdlib.Array.get a 0) in
  Task.parallel_for p ~start:0 ~finish:(n-1) ~body:(fun i -> Stdlib.Array.set res i @@ f i (Stdlib.Array.get a i));
  res

let mapi_inplace f a p =
  let n = Stdlib.Array.length a in 
  Task.parallel_for p ~start:0 ~finish:(n-1) ~body:(fun i -> Stdlib.Array.set a i @@ f i (Stdlib.Array.get a i))
