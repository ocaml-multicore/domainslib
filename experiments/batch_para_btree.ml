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
  Array.sort (fun (k, _) (k', _) -> Int.compare k k') keys;
  (* allocate a buffer for the results *)
  let results: 'a option array = Array.make (Array.length ks) None in
  Domainslib.Task.run pool
    (fun () -> par_search_node pool t.root ~keys ~results ~range:(0, Array.length keys));
  results

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

let[@warning "-27"] build_aux ~max_keys pool (batch : (int * 'a) array) : 'a node = 
  let t = max_keys + 1  in
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
        { n = sz; 
          keys=Array.init sz (fun i -> fst batch.(lo+i));
          values=Array.init sz (fun i -> snd batch.(lo+i));
          leaf = true;
          children = [||];
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
      if node.no_elements < 1000 then 
        begin
          for i = 0 to t-2 do 
            let rstart, rstop = range.(i) in
            let rightmost = i = t - 2 in
            node.children.(i) <- aux rstart (rstop-1) rightmost
          done
        end
      else
        Domainslib.Task.parallel_for pool ~start:0 ~finish:(t-2) ~body:
          (fun i -> 
             let rstart, rstop = range.(i) in
             let rightmost = i = t - 2 in
             node.children.(i) <- aux rstart (rstop-1) rightmost
          ); 
      node
    end
  in
  aux 0 (Array.length batch) false

let[@warning "-27"] build_aux_alt ~max_keys pool (batch : (int * 'a) array) : 'a node = 
  let t = max_keys + 1  in
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
        { n = sz; 
          keys=Array.init sz (fun i -> fst batch.(lo+i));
          values=Array.init sz (fun i -> snd batch.(lo+i));
          leaf = true;
          children = [||];
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
      begin
        for i = 0 to t-2 do 
          let rstart, rstop = range.(i) in
          let rightmost = i = t - 2 in
          node.children.(i) <- aux rstart (rstop-1) rightmost
        done
      end;
      node
    end
  in
  aux 0 (Array.length batch) false

let build ~max_keys pool batch = 
  {max_keys = max_keys + 1;  root = build_aux_alt ~max_keys pool batch}

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

    (* 1. Work out the total size of elements in the merged array
       2. Allocate buffer of this size
       3. Convert kv_array to iter
       4. flatten to iter (Nothing needs to be done)
       5. Write a merge into array (3 params)
    *)
    let batch = Array.make (Array.length kv_arr + t.root.no_elements) kv_arr.(0) in
    let i1 = kv_arr |> Array.to_seq in
    let i2 = flatten t in
    let merged = merge_alt i1 i2 in
    Iter.iteri (fun i vl -> batch.(i) <- vl) merged;
    build_aux ~max_keys pool batch
  end

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

let par_insert_rebuilder ~pool t kv_arr =
  let batch_size = Array.length kv_arr in
  if batch_size >= t.root.no_elements then
    t.root <- par_rebuild ~pool t kv_arr
  else
    Array.iter (fun (k,v) -> insert t k v) kv_arr