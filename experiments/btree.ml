let d = 2
type 'a node = {
  mutable n : int;
  keys : int array; (* Sorted in increasing order *)
  mutable leaf : bool;
  mutable c : 'a node option array option
}

type 'a t = {
  mutable root : 'a node;
  t : int
}

let rec print_root {keys; leaf; c; _} =
  let s_keys =
    Array.fold_left (fun acc k -> acc ^ string_of_int k ^ "; ") "[" keys in
  if leaf then
    Printf.printf "%s" ("Leaf" ^ s_keys)
  else Printf.printf "%s" ("Node" ^ s_keys);
  print_newline ();
  match c with
  | Some c_arr -> Array.iter (fun c -> match c with | Some c -> print_root c | None -> Printf.printf "None ") c_arr
  | None -> Printf.printf "None" 
  

let alloc_node ?(leaf = false) () = { 
  n = 0;
  keys = Array.make (2*d) 0;
  leaf;
  c = None
}

let ( !^ ) = function
  | Some op_arr -> op_arr 
  | None -> failwith "invalid deref"

let get_child node i = match node with
  | Some op_arr -> op_arr.(i) |> Option.get
  | None -> failwith "invalid get"

let rec search x k =
  let i = ref 1 in
  (* Look for key in current node *)
  while !i <= x.n && k > x.keys.(!i) do
    incr i
  done;
  (* Check if current key is the one we want *)
  if !i <= x.n && k = x.keys.(!i) then Some (x, !i)
  (* If not check if the node is not a leaf *)
  else if x.leaf then None
  (* Recursively search the child *)
  else search (get_child x.c !i) k

let create t =
  let x = alloc_node ~leaf:true () in
  {root = x; t}

let split_child (x : 'a node) (i: int) (y: 'a node) =
  let z = alloc_node ~leaf:y.leaf () in
  z.n <- d-1;
  (* Copy keys from left child to right *)
  for j = 1 to d-1 do
    z.keys.(j) <- y.keys.(j+d)
  done;
  if not y.leaf then
    (
      for j = 1 to d do
        let z_c = !^(z.c) in
        z_c.(j) <- !^(y.c).(j+d)
      done
    );
  y.n <- d-1;
  for j = x.n + 1 downto i + 1 do
    let x_c = !^(x.c) in
    x_c.(j+1) <-  x_c.(j)
  done;
  !^(x.c).(i+1) <- Some z;
  for j = x.n downto i do
    x.keys.(j+1) <- x.keys.(j)
  done;
  x.keys.(i) <- y.keys.(d);
  x.n <- x.n + 1

let rec insert_non_full x k =
  let i = ref (x.n) in
  if x.leaf
  then (while !i >= 1 && k < x.keys.(!i) do
          x.keys.(!i+1) <- x.keys.(!i);
          decr i
        done;
        x.keys.(!i+1) <- k;
        x.n <- x.n + 1)
  else (while !i >= 1 && k < x.keys.(!i) do
          decr i
        done;
        incr i;
        let x_c_i = !^(x.c).(!i) |> Option.get in
        if x_c_i.n = 2 * d - 1
        then (split_child x !i x_c_i;
              if k > x.keys.(!i)
              then incr i);
        insert_non_full x_c_i k
       )

let insert t k =
  let r = t.root in
  if r.n = 2*d - 1
  then (
    let s = alloc_node ~leaf:false () in
    t.root <- s;
    s.c <- Some (Array.make (2*d+1) (Some r));
    split_child s 1 r;
    insert_non_full s k;
  )
  else insert_non_full r k

let%test "basic test" =
  let t = create 3 in
  insert t 3;
  insert t 2;
  insert t 1;
  insert t 4;
  print_root t.root;
  let res = search t.root 1 in
  Option.is_some res
  
let%test "search test" = print_endline "HELLo"; false
let%test "insert test" = false
let%test "delete test" = false
