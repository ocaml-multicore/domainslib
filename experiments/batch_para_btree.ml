include Btree

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
  Array.fast_sort (fun (k, _) (k', _) -> Int.compare k k') keys;
  (* allocate a buffer for the results *)
  let results: 'a option array = Array.make (Array.length ks) None in
  Domainslib.Task.run pool
    (fun () -> par_search_node pool t.root ~keys ~results ~range:(0, Array.length keys));
  results

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

(* Ceiling division *)
let ( ^/ ) i1 i2 =
  assert (i1 >= i2);
  assert (i1 >= 0 && i2 >= 0);
  let add = i1 mod i2 > 0 in
  if add then i1/i2 + 1 else i1 / i2

let range : int -> int -> int -> int * (int * int) array =
  fun t lo hi ->
  let d = (hi - lo) ^/ (2*t-1) in
  let sz = (hi - lo) ^/ d in
  sz, (Array.init sz (fun i ->
      let rstart = lo + (i * d) in
      let rstop = min hi (lo + ((i+1) * d)) in
      rstart,rstop))

let pp_range ppf (r : (int * int) array) =
  let pp_tuple ppf tup = Format.fprintf ppf "(%d, %d)" (fst tup) (snd tup) in
  Format.pp_print_list pp_tuple ppf (Array.to_list r)

(*
   Tree needs to maintain these invariants:
   1. Every node has node.(x) number of children pointers
   2. All leaves have the same depth
   3. Lower and upper bound of no. of keys defined by degree t >= 2
   - Every internal node has at least t-1 keys => t children
   - Every node can contain at most 2t - 1 keys
*)
let rec ( ** ) n e =
  if e < 1 then n else n * (n ** (e - 1));;

let[@warning "-27"] build_aux t pool (batch : (int * 'a) array) : 'a node =
  let nil = { n = 0;
              keys=[||];
              values=[||];
              leaf= true;
              children= [||];
              no_elements= 0 } in
  let rec aux lo hi : 'a node =
    let sz = hi - lo in
    if sz <= 2*t-1 then
      begin
        (* Format.printf "[Lo:%d, Hi:%d] LEAF%!\n" lo hi; *)
        { n = sz;
          keys=Array.init sz (fun i -> fst batch.(lo+i));
          values=Array.init sz (fun i -> snd batch.(lo+i));
          leaf = true;
          children = [||];
          no_elements= sz }
      end
    else begin
      let n_keys, range = range t lo hi in
      (* Format.printf "[Lo:%d, Hi:%d] Range = %a\n%!" lo hi pp_range range; *)
      let keys = Array.init n_keys (fun i ->
          (* Printf.printf "%d index\n%!" i; *)
          let idx = (range.(i) |> snd) - 1 in
          fst batch.(idx)) in
      let values = Array.init n_keys (fun i ->
          let idx = (range.(i) |> snd) - 1 in
          snd batch.(idx)) in
      let node = { n = n_keys;
                   keys;
                   values;
                   leaf= false;
                   children= Array.make n_keys nil;
                   no_elements = hi-lo } in
      if sz < 512 then
        begin
          for i = 0 to (n_keys-1) do
            let rstart, rstop = range.(i) in
            node.children.(i) <- aux rstart rstop
          done;
        end
      else
        Domainslib.Task.parallel_for pool ~start:0 ~finish:(n_keys-1) ~body:
          (fun i ->
             let rstart, rstop = range.(i) in
             node.children.(i) <- aux rstart rstop
          );
      node
    end
  in
  aux 0 (Array.length batch)

let build ~max_keys pool batch =
  let t = (max_keys + 1) / 2  in
  {max_keys = max_keys + 1;  root = build_aux t pool batch}

let rec int_range_downto start stop =
  fun () ->
  if start > stop
  then Seq.Nil
  else Seq.Cons (stop, int_range_downto start (stop - 1))

let flatten t =
  let open Seq in
  let rec aux node =
    if node.leaf then
      let elems = Array.map2 (fun k v -> k, v) node.keys node.values in
      Array.to_seq elems
    else begin
      let back =
        int_range_downto 1 (node.n) |>
        fold_left (fun acc i ->
            let tl = aux (node.children.(i)) in
            let kv = node.keys.(i-1), node.values.(i-1) in
            let comb = cons kv tl in
            append comb acc
          ) empty in
      append (aux (node.children.(0))) back
    end
  in
  aux t.root

let[@warning "-32"] par_flatten t pool =
  let sz = t.root.no_elements in
  assert(sz > 0);
  let kv_arr = Array.make sz (0, t.root.values.(0)) in
  let rec aux node lo =
    if node.leaf then
      for i = 0 to node.n - 1 do
        Printf.printf "Filling up %d in slotid %d\n" node.keys.(i) (lo+i);
        kv_arr.(lo+i) <- (node.keys.(i), node.values.(i))
      done
    else begin
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(node.n)
        ~body:(fun i ->
            let lo = ref lo in
            for j = 0 to i-1 do
              lo := !lo + node.children.(j).no_elements
            done;
            let islot = !lo + node.children.(i).no_elements in
            Printf.printf "islot = %d\n" islot;
            if i < node.n then
              kv_arr.(islot) <- node.keys.(i), node.values.(i);
            aux node.children.(i) (!lo)
          )
    end
  in
  aux t.root 0;
  kv_arr

let merge_l l1 l2 =
  let rec walk l1 l2 acc =
    match l1, l2 with
    | [], [] -> acc
    | l , [] | [], l -> List.rev_append l acc
    | h1 :: tl1, h2 :: tl2 ->
      if fst h1 <  fst h2 then
        walk tl1 (h2::tl2) (h1 :: acc) else
        walk (h1 :: tl1) tl2 (h2 :: acc)
  in
  walk l1 l2 [] |> List.rev

let merge i1 i2 =
  let hd_tl i = Iter.head i, Iter.drop 1 i in
  let rec aux acc i1 i2 =
    match hd_tl i1, hd_tl i2 with
    | (None,_), (None, _) -> acc
    | (Some hd1, tl1), (Some hd2, tl2) ->
      if hd1 < hd2
      then aux (Iter.cons hd1 acc) tl1 (Iter.cons hd2 tl2)
      else aux (Iter.cons hd2 acc) (Iter.cons hd2 tl1) tl2
    | (Some _, _), (None, _) -> Iter.append i1 acc
    | (None,_), (Some _, _) -> Iter.append i2 acc
  in
  aux Iter.empty i1 i2 |> Iter.rev |> Iter.to_array

let merge_alt i1 i2 : _ Iter.t =
  let i1 = Seq.to_dispenser i1 in
  let i2 = Seq.to_dispenser i2 in
  let next i h = match h with None -> i () | Some v -> Some v in
  let rec aux i1 h1 i2 h2 f =
    match next i1 h1, next i2 h2 with
    | None,None -> ()
    | (Some hd1, Some hd2) ->
      if hd1 < hd2
      then (f hd1; aux i1 None i2 (Some hd2) f)

      else (f hd2; aux i1 (Some hd1) i2 None f)
    | (Some hd1, None) -> (f hd1; aux i1 None i2 None f)
    | (None, Some hd2) -> (f hd2; aux i1 None i2 None f)
  in
  fun f -> aux i1 None i2 None f


let par_rebuild ~pool (t: 'a t) (kv_arr : (int * 'a) array) =
  if Array.length kv_arr = 0 then t.root
  else begin
    (* keys is a array of (key, index) where index is the position in the original search query *)
    let max_keys = t.max_keys in
    Array.sort (fun (k,_) (k',_) -> Int.compare k k') kv_arr;
    let batch = Array.make (Array.length kv_arr + t.root.no_elements) kv_arr.(0) in
    let i1 = kv_arr |> Array.to_seq in
    let i2 = flatten t in
    let merged = merge_alt i1 i2 in
    Iter.iteri (fun i vl -> batch.(i) <- vl) merged;
    build_aux max_keys pool batch
  end

(*
   1. Need to double check the correctness of the builder, sometimes we end up with leaf nodes with no elements
   2. Profile why the parallelism doesn't speed things up
   3. Work on figuring out the invariant for reasoning about BTree inserts
   - Ensure that the tree does not need grow more than 1 level. If it does, we rebuild
   - At each node we split min(children, batch_size)
*)
let par_insert_rebuilder ~pool t kv_arr =
  let batch_size = Array.length kv_arr in
  if batch_size >= t.root.no_elements then
    t.root <- par_rebuild ~pool t kv_arr
  else
    Array.iter (fun (k,v) -> insert t k v) kv_arr;
