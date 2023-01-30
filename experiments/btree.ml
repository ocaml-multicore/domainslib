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

let keys_fmt = Fmt.(array ~sep:semi int)
let single_node_fmt ppf n = 
  match n.leaf with
  | true -> Fmt.pf ppf "Leaf(%a)" keys_fmt n.keys
  | false -> Fmt.pf ppf "Node(%a)" keys_fmt n.keys

let print_node node = 
  let none ppf () = Fmt.pf ppf "None" in
  let node_op_fmt = Fmt.option ~none single_node_fmt in
  let c_arr_fmt = Fmt.(array ~sep:semi node_op_fmt) in
  let c_arr_op_fmt = Fmt.option c_arr_fmt in
  Fmt.pr "%a\n%a" single_node_fmt node c_arr_op_fmt node.c

let alloc_node ?(leaf = false) () = {
  n = 0;
  keys = Array.make (2*d+1) 0;
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
  (* Fill right child *)
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
        print_node x;
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
    let c = Array.make (2*d+1) None in
    c.(1) <- Some r;
    s.c <- Some c;
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
  (* insert t 1; *)
  print_node t.root;
  let res = search t.root 1 in
  Option.is_some res

(* Node(0; 2; 0; 0; 0)
   Leaf(0; 1; 4; 3; 0); Leaf(0; 1; 4; 3; 0); Leaf(0; 1; 4; 3; 0); Leaf(0; 1; 4; 3;) ; Leaf(0; 1; 4; 3) *)

let%test "search test" = false
let%test "insert test" = false
let%test "delete test" = false
