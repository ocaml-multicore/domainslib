module Make (V: Map.OrderedType) = struct

  let (.!()) x v = Finite_vector.get x v

  module Sequential = struct

    type 'a node = {
      mutable n: int;                       (*  number of keys in node *)
      mutable keys: V.t Finite_vector.t;              (* keys themselves *)
      mutable values: 'a Finite_vector.t;              (* values *)
      leaf: bool;
      mutable children: 'a node Finite_vector.t;
      mutable no_elements: int;             (* number of elements in the node and subtrees  *)
    }

    type 'a t = {
      mutable root: 'a node;
      max_children: int;
    }

    let rec size_node node =
      if node.leaf
      then Finite_vector.length node.values
      else Finite_vector.fold_left (fun acc vl -> acc + size_node vl) 0 node.children


    let rec pp_node ?(pp_child=true) ?(pp_v=fun fmt _ -> Format.fprintf fmt "<opaque>") indent f fmt node =
      let spaces = (String.make indent ' ') in
      Format.fprintf fmt "%snode(n=%d,leaf=%b,no_elts=%d)\n%s - values=[%a]\n%a"
        spaces node.n node.leaf node.no_elements
        spaces (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
                  (fun fmt (k,vl) -> Format.fprintf fmt "%a: %a" pp_v k f vl))
        (List.init node.n (fun i -> (node.keys.!(i), node.values.!(i))))
        (if pp_child then
           Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
             (fun fmt (k, vl) ->
                match k with
                | None -> Format.fprintf fmt "%s - child(k=_):\n%a" spaces (pp_node ~pp_v (indent + 4) f) vl
                | Some key -> Format.fprintf fmt "%s - child(k=%a):\n%a" spaces pp_v key (pp_node ~pp_v (indent + 4) f) vl
             )
         else fun _fmt _vl -> ())
        (List.init (Finite_vector.length node.children) (fun i ->
           ((if i < node.n then Some node.keys.!(i) else None), node.children.!(i))))

    let pp_node_internal = pp_node
    let pp_node ?pp_v f fmt vl = pp_node ?pp_v 0 f fmt vl
    let show_node ?pp_v f vl = Format.asprintf "%a" (pp_node ?pp_v f) vl
    let show_node_no_children ?pp_v f vl = Format.asprintf "%a" (pp_node_internal ?pp_v ~pp_child:false 0 f) vl

    let pp ?pp_v f fmt t =
      pp_node ?pp_v f fmt t.root
    let show ?pp_v f vl = Format.asprintf "%a" (pp ?pp_v f) vl

    let init ?(max_children=3) () =
      let root = {
        n=0;
        leaf=true;
        keys=Finite_vector.init ~capacity:(2 * max_children - 1) ();
        children=Finite_vector.init ~capacity:(2 * max_children) ();
        values=Finite_vector.init ~capacity:(2 * max_children - 1) ();
        no_elements=0;
      } in
      {root; max_children}

    let rec fold_int_range ~start ~stop f acc =
      if start >= stop
      then f acc start
      else
        let acc = f acc start in
        fold_int_range ~start:(start + 1) ~stop f acc

    let rec find_int_range ~start ~stop f =
      if stop < start
      then None
      else if start = stop then f start
      else match f start with
        | None -> find_int_range ~start:(start + 1) ~stop f
        | res -> res 

    let rec find_int_range_dec ~start ~stop f =
      if start < stop
      then None
      else if start = stop then f stop
      else match f start with
        | None -> find_int_range_dec ~start:(start - 1) ~stop f
        | res -> res

    let rec search_node x k =
      let index =
        find_int_range ~start:0 ~stop:(x.n - 1) (fun i ->
          if V.compare k x.keys.!(i) <= 0
          then Some i
          else None)
        |> Option.value ~default:x.n in
      if index < x.n && V.compare x.keys.!(index) k = 0
      then Some (x, index)
      else if x.leaf then None
      else
        search_node (x.children.!(index)) k

    let search t k =
      match search_node t.root k with
      | Some (node, i) -> Some node.values.!(i)
      | None -> None

    (* pre: x.(i) has (2 * t - 1) keys *)
    let split_child x i =
      let y = x.children.!(i) in
      let t = (y.n + 1) / 2 in
      let z =
        let keys = Finite_vector.split_from y.keys t in
        let values = Finite_vector.split_from y.values t in
        let children =
          if y.leaf then Finite_vector.init ~capacity:(2 * t) ()
          else Finite_vector.split_from y.children t in
        { n = t - 1; leaf=y.leaf; keys; values; children; no_elements=0; } in
      z.no_elements <- t - 1;
      Finite_vector.iter (fun child -> z.no_elements <- z.no_elements + child.no_elements) z.children;

      (* insert z *)
      Finite_vector.insert x.keys i y.keys.!(t - 1);
      Finite_vector.insert x.values i y.values.!(t - 1);
      Finite_vector.insert x.children (i + 1) z;

      (* clip y *)
      y.n <- t - 1;
      Finite_vector.clip y.keys (t - 1);
      Finite_vector.clip y.values (t - 1);
      y.no_elements <- t - 1;
      Finite_vector.iter (fun child -> y.no_elements <- y.no_elements + child.no_elements) y.children;

      x.n <- x.n + 1

    let rec insert_node ~max_children x k vl =
      let index =
        find_int_range_dec ~start:(x.n - 1) ~stop:0 (fun i ->
          if V.compare k x.keys.!(i) >= 0
          then Some (i + 1) else None)
        |> Option.value ~default:0 in
      x.no_elements <- x.no_elements + 1;
      if x.leaf
      then begin
        Finite_vector.insert x.keys index k;
        Finite_vector.insert x.values index vl;
        x.n <- x.n + 1;
      end else begin
        if x.children.!(index).n = 2 * max_children - 1
        then begin
          split_child x index;
          if V.compare k x.keys.!(index) > 0
          then insert_node ~max_children x.children.!(index + 1) k vl
          else insert_node ~max_children x.children.!(index) k vl
        end
        else
          insert_node ~max_children x.children.!(index) k vl
      end

    let insert tree k vl =
      let t = tree.max_children in
      let r = tree.root in
      if r.n = 2 * t - 1
      then begin
        let s = {
          n=0;
          leaf=false;
          keys=Finite_vector.init ~capacity:(2 * t - 1) ();
          children=Finite_vector.singleton ~capacity:(2 * t) (tree.root);
          values=Finite_vector.init ~capacity:(2 * t - 1) ();
          no_elements=r.no_elements;
        } in
        tree.root <- s;
        split_child s 0;
        insert_node ~max_children:tree.max_children s k vl
      end else
        insert_node ~max_children:tree.max_children r k vl

  end

  type 'a t = 'a Sequential.t

  type ('elt, 'ret) op =
    | Insert : V.t * 'elt -> ('elt, unit) op
    | Search : V.t -> ('elt, 'elt option) op
    | Size : ('elt, int) op

  type 'a wrapped_op = Mk : ('a, 'b) op * ('b -> unit) -> 'a wrapped_op

  let init () = Sequential.init ~max_children:8 ()

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
    then Sequential.{
      n = stop - start;
      keys = Finite_vector.init_with ~capacity:(2 * t - 1) (stop - start) (fun i -> fst arr.(start + i));
      values=Finite_vector.init_with ~capacity:(2 * t - 1) (stop - start) (fun i -> snd arr.(start + i));
      leaf=true;
      children = Finite_vector.init ~capacity:(2 * t) ();
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
      let keys = Finite_vector.init_with ~capacity:(2 * t - 1) n (fun pos -> fst arr.(key_inds.(pos))) in
      let values = Finite_vector.init_with ~capacity:(2 * t - 1) n (fun pos -> snd arr.(key_inds.(pos))) in
      let children = Finite_vector.init_with ~capacity:(2 * t) (Array.length children)
                       (fun pos -> children.(pos)) in
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
        let keys = Finite_vector.init_with ~capacity:(2 * t - 1) n (fun pos -> fst arr.(key_inds.(pos))) in
        let values = Finite_vector.init_with ~capacity:(2 * t - 1) n (fun pos -> snd arr.(key_inds.(pos))) in
        let children = Finite_vector.init_with ~capacity:(2 * t) (Array.length children)
                         (fun pos -> children.(pos)) in
        { n; keys; values; leaf=false; children; no_elements=Array.length arr } in
    Sequential.{
      root;
      max_children=t
    }


  let rec par_search_node : Domainslib.Task.pool -> 'a Sequential.node ->
    keys:(V.t * int) array -> results:'a option array -> range:(int * int)
    -> unit =
    fun pool node ~keys ~results ~range:(rstart, rstop) ->
    (* if the no elements in the node are greater than the number of keys we're searching for, then just do normal search in parallel *)
    if node.no_elements > (rstop - rstart) && false then
      Domainslib.Task.parallel_for pool ~start:rstart ~finish:(rstop - 1) ~body:(fun i ->
        let (k,ind) = keys.(i) in
        results.(ind) <- Option.map (fun (node,i) -> node.Sequential.values.!(i)) (Sequential.search_node node k)
      )
    else begin
      let handle_equal_keys ki i =
        if i < node.n && fst keys.(ki) = node.keys.!(i) then
          results.(snd keys.(ki)) <- Some node.values.!(i) in
      (* partition children by index they belong to  *)
      let children =
        Sequential.fold_int_range ~start:rstart ~stop:(rstop - 1)
          (fun (acc, ks, i) ki ->
             (* ks - the start of the current index, i - the current key of the node we're checking   *)
             (* if we haven't handled all keys  *)
             if i < node.n then begin
               let acc, ks, i = if fst keys.(ki) <= node.keys.!(i)
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
          par_search_node pool node.children.!(i) ~keys ~results ~range:children.(i)
        );
    end

  let par_search ~pool (t: 'a t) ks =
    (* keys is a array of (key, index) where index is the position in the original search query *)
    let keys = Array.mapi (fun ind ks -> (ks, ind)) ks in
    Array.fast_sort (fun (k, _) (k', _) -> V.compare k k') keys;
    (* allocate a buffer for the results *)
    let results: 'a option array = Array.make (Array.length ks) None in
    Domainslib.Task.run pool
      (fun () -> par_search_node pool t.root ~keys ~results ~range:(0, Array.length keys));
    results


  let run (type a) (t: a t) (pool: Domainslib.Task.pool) (ops: a wrapped_op array) : unit =
    let searches : (V.t * (a option -> unit)) list ref = ref [] in
    let inserts : (V.t * a) list ref = ref [] in
    let start_size = t.root.no_elements in
    Array.iter (fun (elt: a wrapped_op) -> match elt with
      | Mk (Insert (key,vl), kont) -> kont (); inserts := (key,vl) :: !inserts
      | Mk (Search key, kont) -> searches := (key, kont) :: !searches
      | Mk (Size,kont) -> kont start_size
    ) ops;
    let searches = Array.of_list !searches in
    Domainslib.Task.parallel_for pool ~start:0 ~finish:(Array.length searches - 1) ~body:(fun i ->
      let key, kont = searches.(i) in
      kont (Sequential.search t key)
    );
    let inserts = Array.of_list !inserts in
    for i = 0 to Array.length inserts - 1 do
      let k,v = inserts.(i) in
      Sequential.insert t k v
    done

end
