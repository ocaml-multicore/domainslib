
type 'a node = {
  mutable n: int;                       (*  number of keys in node *)
  mutable keys: int array;              (* keys themselves *)
  mutable values: 'a array;              (* values *)
  leaf: bool;
  mutable children: 'a node array;
}

type 'a t = {
  mutable root: 'a node;
  max_keys: int;
}

let rec size_node node =
  if node.leaf
  then Array.length node.values
  else Array.fold_left (fun acc vl -> acc + size_node vl) 0 node.children
  
let rec pp_node indent f fmt node =
  let spaces = (String.make indent ' ') in
  Format.fprintf fmt "%snode(n=%d,leaf=%b)\n%s - values=[%a]\n%a"
    spaces node.n node.leaf
    spaces (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
              (fun fmt (k,vl) -> Format.fprintf fmt "%d: %a" k f vl))
    (List.init node.n (fun i -> (node.keys.(i), node.values.(i))))
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
       (fun fmt (k, vl) ->
          match k with
          | None -> Format.fprintf fmt "%s - child(k=_):\n%a" spaces (pp_node (indent + 4) f) vl
          | Some key -> Format.fprintf fmt "%s - child(k=%d):\n%a" spaces key (pp_node (indent + 4) f) vl
       ))
    (List.init (Array.length node.children) (fun i -> ((if i < node.n then Some node.keys.(i) else None), node.children.(i))))
let pp_node_internal = pp_node
let pp_node f fmt vl = pp_node 0 f fmt vl
let show_node f vl = Format.asprintf "%a" (pp_node f) vl

  
let pp f fmt t =
  pp_node f fmt t.root
let show f vl = Format.asprintf "%a" (pp f) vl


let create ?(max_keys=3) () =
  let root = {
    n=0;
    leaf=true;
    keys=[| |];
    children=[| |];
    values=[| |]
  } in
  {root; max_keys}

let rec search_node x k =
  let index = Iter.int_range ~start:0 ~stop:(x.n - 1)
              |> Iter.find (fun i ->
                if k <= x.keys.(i)
                then Some i
                else None)
              |> Option.value ~default:x.n in
  if index < x.n && x.keys.(index) = k
  then Some (x, index)
  else if x.leaf then None
  else
    search_node (x.children.(index)) k


let search t k =
  match search_node t.root k with
  | Some (node, i) -> Some node.children.(i)
  | None -> None

let make_safe i vl =
  if i <= 0 then [| |] else Array.make i (vl ())

let resize_node y t =
  y.n <- t - 1;
  y.keys <- Array.init (t - 1) (fun j -> y.keys.(j));
  y.values <- Array.init (t - 1) (fun j -> y.values.(j));
  if not y.leaf then
    y.children <- Array.init t (fun j -> y.children.(j))


(* pre: x.(i) has (2 * t - 1) keys *)
let split_child x i =
  let y = x.children.(i) in
  let t = (y.n + 1) / 2 in
  let z = {
    n = t - 1;
    leaf=y.leaf;
    keys=Array.init (t - 1) (fun j -> y.keys.(j + t));
    values=Array.init (t - 1) (fun j -> y.values.(j + t));
    children=if y.leaf then [| |] else Array.init t (fun j -> y.children.(j + t))
  } in
  (* update x *)
  x.keys <- Array.init (x.n + 1) (fun j ->
    if j < i then x.keys.(j)
    else if j = i then y.keys.(t - 1)
    else x.keys.(j - 1)
  );
  x.values <- Array.init (x.n + 1) (fun j ->
    if j < i then x.values.(j)
    else if j = i then y.values.(t - 1)
    else x.values.(j - 1)
  );
  x.children <- Array.init (x.n + 2) (fun j ->
    if j <= i then x.children.(j)
    else if j = i + 1 then z
    else x.children.(j - 1)
  );
  x.n <- x.n + 1;
  (* clip y *)
  resize_node y t

let rec insert_node ~max_size x k vl =
  let index =
    Iter.int_range_dec ~start:(x.n - 1) ~stop:0
    |> Iter.find (fun i -> if k >= x.keys.(i) then Some (i + 1) else None)
    |> Option.value ~default:0 in
  if x.leaf
  then begin
    x.keys <- Array.init (x.n + 1) (fun i ->
      if i < index
      then x.keys.(i)
      else if i = index then k
      else x.keys.(i - 1)
    );
    x.values <- Array.init (x.n + 1) (fun i ->
      if i < index
      then x.values.(i)
      else if i = index then vl
      else x.values.(i - 1)
    );
    x.n <- x.n + 1
  end else begin
    if x.children.(index).n = 2 * max_size - 1
    then begin
      split_child x index;
      if k > x.keys.(index)
      then insert_node ~max_size x.children.(index + 1) k vl
      else insert_node ~max_size x.children.(index) k vl
    end
    else
      insert_node ~max_size x.children.(index) k vl
  end

let insert tree k vl =
  let t = tree.max_keys in
  let r = tree.root in
  if r.n = 2 * t - 1
  then begin
    let s = {
      n=0;
      leaf=false;
      keys=[| |];
      children=[| r |];
      values=[| |]
    } in
    tree.root <- s;
    split_child s 0;
    insert_node ~max_size:tree.max_keys s k vl
  end else
    insert_node ~max_size:tree.max_keys r k vl

