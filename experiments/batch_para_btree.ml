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

let[@warning "-27"] build ~max_keys pool (batch : (int * 'a) array) : 'a t = 
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
        (*         let leaf, children = if rightmost && sz = t-1 then 
                    false, Array.make t nil else true, [||] in
        *)
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
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(t-2) ~body:
        (fun i -> 
           let rstart, rstop = range.(i) in
           let rightmost = i = t - 2 in
           node.children.(i) <- aux rstart (rstop-1) rightmost
        ); node
    end
  in
  {max_keys = t; root = aux 0 (Array.length batch) false}

let flatten : 'a t -> (int * 'a) array =
  fun t -> [||]


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