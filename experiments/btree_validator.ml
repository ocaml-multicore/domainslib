include Btree

(* Invariant

   Node properties:
   1. Keys in a node are ordered in increasing order
   2. Every internal node has at least t-1 keys
   3. Every node can contain at most 2t-1 keys
   4. Every internal node contains n_keys + 1 children pointers and leaf nodes have no children
   5. Key ranges are k1 <= key1[x] <= k2 ... <= key2[x] ...

   Tree properties
   1. All leaves have the same depth

 *)

module ISet = Set.Make(Int)

let validate_node (node : 'a node) t height : bool =
  let n_keys = Array.length node.keys in
  let n_children = Array.length node.children in
  if node.leaf then
    n_children = 0
  else begin
    let (p1,_) = Array.fold_left (fun (b, prev_v) v -> b && prev_v <= v, v) (true, min_int) node.keys in
    let p2 = if height = 0 then true else n_keys >= t-1 in
    let p3 = n_keys <= 2*t-1 in
    let p4 = n_children = n_keys + 1 in
    if p1 && p2 && p3 && p4 then true else
      (Format.printf "\nIncreasing Keys (%b) | At least t-1 keys (%b) | Contains at most 2t-1 keys (%b) | Correct no. children (%b)"
        p1 p2 p3 p4; false)
    end

(* Check that all leaves have the same depth, check node properties at the same time *)
let validate_tree (t : 'a Btree.t) : bool =
  let degree = t.max_keys in
  let rec walk h node =
    assert(validate_node node degree h);
    if node.leaf then h else
      let heights = Array.map (fun child -> walk (h+1) child) node.children in
      let heights_set = Array.to_seq heights |> ISet.of_seq in
      if ISet.cardinal heights_set = 1 then ISet.choose heights_set else -1
  in
  walk 0 t.root >= 0

let validate (t : 'a Btree.t) : bool = validate_tree t
