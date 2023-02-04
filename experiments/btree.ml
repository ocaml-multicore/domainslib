
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
  max_keys: int;
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


let create ?(max_keys=3) () =
  let root = {
    n=0;
    leaf=true;
    keys=[| |];
    children=[| |];
    values=[| |];
    no_elements=0;
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
  | Some (node, i) -> Some node.values.(i)
  | None -> None

let rec par_search_node : Domainslib.Task.pool ->
  'a node -> keys:(int * int) array -> results:'a option array -> range:(int * int)
  -> unit =
  fun pool node ~keys ~results ~range:(rstart, rstop) ->
  (* if the no elements in the node are greater than the number of keys we're searching for, then just do normal search in parallel *)
  if node.no_elements > (rstop - rstart) && false then
    Domainslib.Task.parallel_for pool ~start:rstart ~finish:(rstop - 1) ~body:(fun i ->
        let (k,ind) = keys.(i) in
        results.(ind) <- Option.map (fun (node,i) -> node.values.(i)) (search_node node k)
      )
  else begin
    let handle_equal_keys ki i =
      if i < node.n && fst keys.(ki) = node.keys.(i) then 
        results.(snd keys.(ki)) <- Some node.values.(i) in
    (* partition children by index they belong to  *)
    let children =
      Iter.int_range ~start:rstart ~stop:(rstop - 1)
      |> Iter.fold (fun (acc, ks, i) ki ->
          (* ks - the start of the current index, i - the current key of the node we're checking   *)
          (* if we haven't handled all keys  *)
          if i < node.n then begin
            let acc, ks, i = if fst keys.(ki) <= node.keys.(i)
              then (acc, ks, i) 
              else ((ks, ki) :: acc, ki, i + 1) in
            handle_equal_keys ki i;
            (acc,ks,i)
          end else (acc, ks, i)
        ) ([], rstart, 0)
      |> (fun (acc, ks, _) -> (ks,rstop) :: acc)
      |> List.rev
      |> Array.of_list in
    if not node.leaf then
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(Array.length children - 1) ~body:(fun i ->
          par_search_node pool node.children.(i) ~keys ~results ~range:children.(i)
        );
  end

let par_search ~pool (t: 'a t) ks =

  (* keys is a array of (key, index) where index is the position in the original search query *)
  let keys = Array.mapi (fun ind ks -> (ks, ind)) ks in
  Array.sort (fun (k, _) (k', _) -> Int.compare k k') keys;
  (* allocate a buffer for the results *)
  let results: 'a option array = Array.make (Array.length ks) None in
  Domainslib.Task.run pool
    (fun () -> par_search_node pool t.root ~keys ~results ~range:(0, Array.length keys));

  results

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

let rec insert_node ~max_size x k vl =
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
      values=[| |];
      no_elements=r.no_elements;
    } in
    tree.root <- s;
    split_child s 0;
    insert_node ~max_size:tree.max_keys s k vl
  end else
    insert_node ~max_size:tree.max_keys r k vl

(* Ceiling division *)
let ( ^/ ) i1 i2 = 
  assert (i1 >= i2);
  assert (i1 >= 0 && i2 >= 0);
  let add = i1 mod i2 > 0 in
  if add then i1/i2 + 1 else i1 / i2

let range : int -> int -> int -> int -> (int * int) array =
  fun t d lo hi -> 
  let a = Array.init (t-1) (fun i -> 
      let rstart = lo + (i * d) in
      let rstop = lo + ((i+1)*d) in
      rstart,rstop
    ) in
  a.(t-2) <- (fst a.(t-2), hi);
  a

let pp_range ppf (r : (int * int) array) = 
  let pp_tuple ppf tup = Format.fprintf ppf "(%d, %d)" (fst tup) (snd tup) in
  Format.pp_print_list pp_tuple ppf (Array.to_list r)

let[@warning "-27"] build t pool (batch : (int * 'a) array) : 'a t = 
  let nil = { n = 0; 
              keys=[||];
              values=[||];
              leaf= true;
              children= [||];
              no_elements= 0 } in
  let rec aux lo hi rightmost : 'a node =
    let sz = hi - lo in
    if sz <= t-1 && not (rightmost && sz = t-1) then
      begin
        (* This part is probably redundant *)
        let leaf, children = if rightmost && sz = t-1 then 
            false, Array.make t nil else true, [||] in

        { n = sz; 
          keys=Array.init sz (fun i -> fst batch.(lo+i));
          values=Array.init sz (fun i -> snd batch.(lo+i));
          leaf;
          children;
          no_elements= sz }
      end
    else begin
      let d = sz ^/ t-1 in
      let range = range t d lo (hi+1) in
      let keys = Array.init (t-2) (fun i -> 
          let idx = (range.(i) |> snd) - 1 in
          fst batch.(idx)) in
      let values = Array.init (t-2) (fun i -> 
          let idx = (range.(i) |> snd) - 1 in
          snd batch.(idx)) in
      let node = { n = t-2; 
                   keys;
                   values;
                   leaf= false;
                   children= Array.make (t-1) nil;
                   no_elements = hi-lo } in
      (* Need to wrap in a Task.run *)
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(t-2) ~body:
        (fun i -> 
           let rstart, rstop = range.(i) in
           let rightmost = i = t - 2 in
           node.children.(i) <- aux rstart (rstop-1) rightmost
        ); node
    end
  in
  {max_keys = t; root = aux 0 (Array.length batch) false}

let rec par_insert_node : Domainslib.Task.pool -> 'a node ->
  key_vals:((int * 'a) * int) array -> 
  range:(int * int) -> 
  max_keys:int -> 
  unit =
  fun pool node ~key_vals ~range:(rstart, rstop) ~max_keys ->
  (* if the no elements in the node are greater than the number of keys we're searching for, then just do normal search in parallel *)
  match rstop - rstart with
  | 1 -> 
    let k,v = fst key_vals.(rstart) in
    insert_node ~max_size:max_keys node k v
  (* | n when n >= node.no_elements -> failwith "Rebuild" *)
  | n -> assert(n > 0);
    begin
      let[@warning "-27"] _handle_equal_keys ki i =
        failwith "Handle equal keys"
        (* if i < node.n && fst keys.(ki) = node.keys.(i) then 
           results.(snd keys.(ki)) <- Some node.values.(i) in *)
        (* partition children by index they belong to  *)
      in
      let children =
        Iter.int_range ~start:rstart ~stop:(rstop - 1)
        |> Iter.fold (fun (acc, ks, i) ki ->
            (* ks - the start of the current index, i - the current key of the node we're checking   *)
            (* if we haven't handled all keys  *)
            if i < node.n then begin
              let acc, ks, i = if fst (fst key_vals.(ki)) <= node.keys.(i)
                then (acc, ks, i) 
                else ((ks, ki) :: acc, ki, i + 1) in
              (* handle_equal_keys ki i; *)
              (acc,ks,i)
            end else (acc, ks, i)
          ) ([], rstart, 0)
        |> (fun (acc, ks, _) -> (ks,rstop) :: acc)
        |> List.rev
        |> Array.of_list in
      if not node.leaf then
        Domainslib.Task.parallel_for pool ~start:0 ~finish:(Array.length children - 1) ~body:(fun i ->
            par_insert_node pool node.children.(i) ~key_vals ~range:children.(i) ~max_keys
          )
      else begin
        for i = rstart to rstop - 1 do
          let k,v = fst key_vals.(i) in
          insert_node ~max_size:max_keys node k v
        done
      end
    end

let[@warning "-27"] par_insert ~pool (t: 'a t) (kv_arr : (int * 'a) array) =
  (* keys is a array of (key, index) where index is the position in the original search query *)
  let max_keys = t.max_keys in
  let key_vals = Array.mapi (fun ind (ks, v) -> ((ks,v), ind)) kv_arr in
  Array.sort (fun ((k,_), _) ((k',_), _) -> Int.compare k k') key_vals;
  (* allocate a buffer for the results *)
  Domainslib.Task.run pool
    (fun () -> par_insert_node pool t.root ~key_vals ~range:(0, Array.length key_vals) ~max_keys)