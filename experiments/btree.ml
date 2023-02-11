
type 'a node = {
  mutable n: int;                       (*  number of keys in node *)
  mutable keys: int array;              (* keys themselves *)
  mutable values: 'a array;              (* values *)
  leaf: bool;
  mutable children: 'a node array;
  mutable no_elements: int;             (* number of elements in the node and subtrees  *)
}

type 'a t = {
  mutable root: 'a node;
  max_children: int;
}

let rec size_node node =
  if node.leaf
  then Array.length node.values
  else Array.fold_left (fun acc vl -> acc + size_node vl) 0 node.children

let rec pp_node ?(pp_child=true) indent f fmt node =
  let spaces = (String.make indent ' ') in
  Format.fprintf fmt "%snode(n=%d,leaf=%b,no_elts=%d)\n%s - values=[%a]\n%a"
    spaces node.n node.leaf node.no_elements
    spaces (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
              (fun fmt (k,vl) -> Format.fprintf fmt "%d: %a" k f vl))
    (List.init node.n (fun i -> (node.keys.(i), node.values.(i))))
    (if pp_child then
       Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
         (fun fmt (k, vl) ->
            match k with
            | None -> Format.fprintf fmt "%s - child(k=_):\n%a" spaces (pp_node (indent + 4) f) vl
            | Some key -> Format.fprintf fmt "%s - child(k=%d):\n%a" spaces key (pp_node (indent + 4) f) vl
         )
     else fun _fmt _vl -> ())
    (List.init (Array.length node.children) (fun i -> ((if i < node.n then Some node.keys.(i) else None), node.children.(i))))
let pp_node_internal = pp_node
let pp_node f fmt vl = pp_node 0 f fmt vl
let show_node f vl = Format.asprintf "%a" (pp_node f) vl
let show_node_no_children f vl = Format.asprintf "%a" (pp_node_internal ~pp_child:false 0 f) vl

let pp f fmt t =
  pp_node f fmt t.root
let show f vl = Format.asprintf "%a" (pp f) vl


let create ?(max_children=3) () =
  let root = {
    n=0;
    leaf=true;
    keys=[| |];
    children=[| |];
    values=[| |];
    no_elements=0;
  } in
  {root; max_children}

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
  | Some (node, i) -> Some node.values.(i)
  | None -> None

let make_safe i vl =
  if i <= 0 then [| |] else Array.make i (vl ())

let resize_node y t =
  y.n <- t - 1;
  y.keys <- Array.init (t - 1) (fun j -> y.keys.(j));
  y.values <- Array.init (t - 1) (fun j -> y.values.(j));
  y.no_elements <- (t - 1);
  if not y.leaf then begin
    y.children <- Array.init t (fun j ->
        y.no_elements <- y.no_elements + y.children.(j).no_elements;
        y.children.(j)
      );
  end

(* pre: x.(i) has (2 * t - 1) keys *)
let split_child x i =
  let y = x.children.(i) in
  let t = (y.n + 1) / 2 in
  let z = {
    n = t - 1;
    leaf=y.leaf;
    keys=Array.init (t - 1) (fun j -> y.keys.(j + t));
    values=Array.init (t - 1) (fun j -> y.values.(j + t));
    children=if y.leaf then [| |] else Array.init t (fun j -> y.children.(j + t));
    no_elements=0;
  } in
  z.no_elements <- t - 1;
  Array.iter (fun child -> z.no_elements <- z.no_elements + child.no_elements) z.children;
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

let rec insert_node ~max_children x k vl =
  let index =
    Iter.int_range_dec ~start:(x.n - 1) ~stop:0
    |> Iter.find (fun i -> if k >= x.keys.(i) then Some (i + 1) else None)
    |> Option.value ~default:0 in
  x.no_elements <- x.no_elements + 1;
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
    x.n <- x.n + 1;
  end else begin
    if x.children.(index).n = 2 * max_children - 1
    then begin
      split_child x index;
      if k > x.keys.(index)
      then insert_node ~max_children x.children.(index + 1) k vl
      else insert_node ~max_children x.children.(index) k vl
    end
    else
      insert_node ~max_children x.children.(index) k vl
  end

let insert tree k vl =
  let t = tree.max_children in
  let r = tree.root in
  if r.n = 2 * t - 1
  then begin
    let s = {
      n=0;
      leaf=false;
      keys=[| |];
      children=[| r |];
      values=[| |];
      no_elements=r.no_elements;
    } in
    tree.root <- s;
    split_child s 0;
    insert_node ~max_children:tree.max_children s k vl
  end else
    insert_node ~max_children:tree.max_children r k vl

let fold_left_map f accu l =
  let rec aux accu l_accu = function
    | [] -> accu, List.rev l_accu
    | x :: l ->
      let accu, x = f accu x in
      aux accu (x :: l_accu) l in
  aux accu [] l

let drop_last ls =
  let rec loop acc last = function
    | [] -> List.rev acc
    | h :: t -> loop (last :: acc) h t in
  match ls with
  | [] -> []
  | h :: t -> loop [] h t

let int_pow x y =
  let rec loop acc x y =
    if y > 0 then
      match y mod 2 with
      | 0 -> loop acc (x * x) (y / 2)
      | _ -> loop (acc * x) x (y - 1)
    else
      acc in
  loop 1 x y

let find_height ~t ~no_elts =
  if no_elts < 2 * t - 1
  then 1
  else
    let rec loop t no_elts h t_h t2_h =
      if t_h - 1 <= no_elts && no_elts <= t2_h - 1
      then h
      else
        let t_h_1 = t_h * t and t2_h_1 = t2_h * (2 * t) in
        if t2_h - 1 < no_elts && no_elts < t2_h_1 - 1
        then h + 1
        else loop t no_elts (h+1) t_h_1 t2_h_1 in
    loop t no_elts 1 t (2 * t)

let find_split ?(root=false) ~t ~h r =
  let max_t = 2 * t in
  let min_size = int_pow t (h - 1) - 1 in
  let max_size = int_pow (2 * t) (h - 1) - 1 in
  let rec loop min_size max_size t =
    assert (t <= max_t);
    let elt_size = Int.div (r - t + 1) t in
    let rem_size = Int.rem (r - t + 1) elt_size in
    if min_size <= elt_size && elt_size <= max_size &&
       (rem_size = 0 || elt_size + 1 <= max_size)
    then (t, elt_size, rem_size)
    else loop min_size max_size (t + 1) in
  loop min_size max_size (if root then 2 else t)

let partition_range ?root ~t ~h (start,stop) =
  let t, sub_range_size, rem = find_split ?root ~t ~h (stop - start) in
  let key_inds = Array.make (t - 1) 0 in
  let child_inds = Array.make t 0 in
  let rem = ref rem in
  let start = ref start in
  for i = 0 to t - 1 do
    let rem_comp = if !rem > 0 then (decr rem; 1) else 0 in
    child_inds.(i) <- min (!start + sub_range_size + rem_comp) stop;
    if i < t - 1 then
      key_inds.(i) <- !start + sub_range_size + rem_comp;
    start := !start + sub_range_size + rem_comp + 1;
  done;
  key_inds, child_inds

let rec build_node ~max_children:t ~h start stop arr =
  if h <= 1
  then {
    n = stop - start;
    keys = Array.init (stop - start) (fun i -> fst arr.(start + i));
    values=Array.init (stop - start) (fun i -> snd arr.(start + i));
    leaf=true;
    children = [| |];
    no_elements=stop - start;
  }
  else
    let key_inds, sub_ranges = partition_range ~t ~h (start,stop) in

    let children =
      let start = ref start in
      Array.map (fun stop ->
        let subtree = build_node ~max_children:t ~h:(h - 1) !start stop arr in
        start := (stop + 1);
        subtree
      ) sub_ranges in
    let n = Array.length key_inds in
    let keys = Array.make n 0 in
    let values = Array.make n (snd arr.(start)) in
    Array.iteri (fun pos i ->
      keys.(pos) <- fst arr.(i);
      values.(pos) <- snd arr.(i);
    ) key_inds;
    {
      n;
      keys;
      values;
      leaf=false;
      children;
      no_elements=stop - start
    }

let build_from_sorted ?max_children:(t=3) arr =
  let h = find_height ~t ~no_elts:(Array.length arr) in
  let root =
    if Array.length arr <= 2 * t - 1
    then build_node ~max_children:t ~h:1 0 (Array.length arr) arr
    else
      let key_inds, sub_ranges = partition_range ~root:true ~t ~h (0,(Array.length arr)) in

      let children =
        let start = ref 0 in
        Array.map (fun stop ->
          let subtree = build_node ~max_children:t ~h:(h - 1) !start stop arr in
          start := stop + 1;
          subtree
        ) sub_ranges in
      let n = Array.length key_inds in
      let keys = Array.make n 0 in
      let values = Array.make n (snd arr.(0)) in
      Array.iteri (fun pos i ->
        keys.(pos) <- fst arr.(i);
        values.(pos) <- snd arr.(i);
      ) key_inds;
      { n; keys; values; leaf=false; children; no_elements=Array.length arr } in
  {
    root;
    max_children=t
  }
