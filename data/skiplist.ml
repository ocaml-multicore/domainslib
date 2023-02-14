module Make (V : Stdlib.Map.OrderedType) = struct

  module Sequential = struct

    type t = {
      hdr : node;
      level : int ref;
      maxlevel : int;
      nil : node
    } 
    and node = Hd of node array | Node of data | Null
    and data = {mutable value : V.t; forward : node array}

    let show to_string = function
      | Hd _ -> "Hd"
      | Node {value; _} -> Printf.sprintf "Node(%s)" (to_string value)
      | Null -> "Null"

    let[@warning "-32"] to_string to_string = function
      | Hd forward -> "Hd -> [|" ^(Array.fold_right (fun node acc -> acc ^ "; " ^ (show to_string node)) forward "")^"|]"
      | Node {forward;_} as n ->
        (show to_string n)^"-> [|"^
        (Array.fold_right (fun node acc -> acc ^ "; " ^ (show to_string node)) forward "")
        ^"|]"
      | Null -> "Null"

    let ( !> ) = function
      | Null -> failwith "[!>] Tried to dereference Null"
      | Hd forward | Node {forward; _} -> forward

    let ( !^ ) = function
      | Null -> failwith "[!^] Tried to dereference Null"
      | Hd _ -> failwith "[!^] Tried to dereference Hdr"
      | Node r -> r

    let ( *= ) v1 v2 = V.compare v1 v2 = 0
    let ( *< ) v1 v2 = V.compare v1 v2 = -1

    let compare n1 n2 =
      match n1, n2 with
      | Null, Null -> assert(n1 == n2); 0
      | _, Null -> -1
      | Null, _ -> 1
      | Hd r1, Hd r2 -> assert(r1 == r2); 0
      | Hd _, _ -> -1
      | _, Hd _ -> 1
      | Node d1, Node d2 -> V.compare (d1.value) (d2.value)

    let rec log2 n =
      if n <= 1 then 0 else 1 + (log2 (n asr 1))

    let init ~size () =
      let maxlevel = log2 size in
      let nil = Null in
      { hdr = Hd (Array.make (maxlevel+1) nil); level = ref 0; maxlevel; nil}

    let random_level t =
      let lvl = ref 0 in
      while (Random.float 1.) < 0.5 && !lvl < t.maxlevel do
        incr lvl
      done;
      !lvl

    let make_node t lvl value = Node { value; forward = Array.make (lvl + 1) t.nil}

    let mem t elt =
      let x = ref t.hdr in
      for i = !(t.level) downto 0 do
        while
          match (!>(!x)).(i) with
          | Null | Hd _ -> false
          | Node {value ; _} -> value *< elt
        do
          x := (!>(!x)).(i)
        done
      done;
      x := !>(!x).(0);
      match !x with
      | Null | Hd _ -> false
      | Node {value; _} -> value *= elt

    let insert t elt =
      (* Search for Node *)
      let update = Array.make (t.maxlevel + 1) t.nil in
      let x = ref t.hdr in
      for i = !(t.level) downto 0 do
        while
          match (!>(!x)).(i) with
          | Null | Hd _ -> false
          | Node {value ; _} -> value *< elt
        do
          x := !>(!x).(i)
        done;
        update.(i) <- !x
      done;
      let x = !>(!x).(0) in
      (* Check if we are at the correct point *)
      if
        match x with
        | Null | Hd _ -> false
        | Node {value; _} -> value *= elt
      then (!^x).value <- elt
      else
        let lvl = random_level t in
        if lvl > !(t.level) then (
          for i = !(t.level) + 1 to lvl do
            update.(i) <- t.hdr
          done;
          t.level := lvl);
        let x = make_node t lvl elt in
        for i = 0 to lvl do
          !>x.(i) <- !>(update.(i)).(i);
          !>(update.(i)).(i) <- x
        done

    let size t =
      let rec aux acc = function
        | Null -> acc
        | Hd forward ->
          aux acc forward.(0)
        | Node {forward; _} ->
          aux (acc+1) forward.(0)
      in
      aux 0 t.hdr

    (* Does not work on empty lists *)
    let validate ?(to_string=fun _ -> "<opaque>") t =
      let rec walk prev = function
        | Null -> ()
        | Hd forward -> walk prev forward.(0)
        | Node {value; forward; _} ->
          let vals = value |> to_string in
          let prevs = prev |> to_string in
          (if value < prev then Printf.printf "Ordering error %s -> %s\n" vals prevs
           else if value = prev then Printf.printf "Duplicate error %s -> %s\n" vals prevs);
          walk value forward.(0)
      in
      let starting_point =  !>(t.hdr).(0) in
      let first_val = !^starting_point.value in
      walk first_val !>starting_point.(0)

  end

  type t = Sequential.t

  type 'a op =
    | Insert : V.t -> unit op
    | Member: V.t -> bool op
    | Size: int op

  type wrapped_op = Mk : 'a op * ('a -> unit) -> wrapped_op

  let init () = Sequential.init ~size:10 ()

  type intermediate = {
    batch_size : int;
    maxinsertlevel : int ref;
    level_arr : int array;
    new_node_arr : Sequential.node array;
    new_node_back_arr : Sequential.node array;
    prev_node_idx : int array
  }

  let build_node t idx elem {maxinsertlevel;
                             new_node_arr;
                             new_node_back_arr;
                             level_arr; _} =
    let rdm_level = Sequential.random_level t in
    level_arr.(idx) <- rdm_level;
    if rdm_level > !maxinsertlevel then maxinsertlevel := rdm_level;
    let new_node = Sequential.make_node t rdm_level elem in
    let new_node_back = Sequential.make_node t rdm_level elem in
    new_node_arr.(idx) <- new_node;
    new_node_back_arr.(idx) <- new_node_back

  let relate_nodes t idx {batch_size;
                          maxinsertlevel;
                          level_arr;
                          new_node_arr;
                          new_node_back_arr;
                          prev_node_idx; _} =
    let exception Break in
    let node = new_node_arr.(idx) in
    let next = ref (idx + 1) in
    for lvl = 0 to level_arr.(idx) do
      Sequential.((!>node).(lvl) <- t.nil);
      (try
         for id = !next to (batch_size-1) do
           if lvl <= level_arr.(id) then
             begin
               (* Set forward pointer *)
               Sequential.((!>node).(lvl) <- new_node_arr.(id));
               (* Set back_pointer *)
               Sequential.((!>(new_node_back_arr.(id))).(lvl) <- node);
               (* Set the previous pointer *)
               prev_node_idx.(((!maxinsertlevel+1)*id)+lvl) <- idx;
               (* Update next id to start from *)
               next := id;
               raise Break
             end;
         done
       with Break -> ());
    done

  let merge_list t idx {maxinsertlevel;
                        new_node_arr;
                        level_arr;
                        new_node_back_arr;
                        prev_node_idx; _} =
    let open Sequential in
    let exception Return in
    let node = new_node_arr.(idx) in
    let node_back = new_node_back_arr.(idx) in
    let update = Array.make (t.Sequential.maxlevel + 1) t.nil in
    let x = ref t.hdr in
    try
      for i = !(t.level) downto 0 do
        while
          match Sequential.(!>(!x)).(i) with
          | Null | Hd _ -> false
          | Node {value ; _} -> Sequential.(value *< (!^node).value)
        do
          x := Sequential.(!>(!x).(i))
        done;
        (* No duplicates *)
        (match !x with
         | Null -> ()
         | Hd forward | Node {forward;_} ->
           if forward.(i) != t.nil &&
              (!^(forward.(i))).value *= (!^node).value
           then raise Return);

        update.(i) <- !x
      done;

      for i = 0 to level_arr.(idx) do

        if (!>node).(i) == t.nil || compare ((!>(update.(i))).(i)) ((!>node).(i)) <= 0 then
          (if (!>(update.(i))).(i) != t.nil then
             (!>node).(i) <- (!>(update.(i))).(i));

        let prev_node_id = prev_node_idx.(((!maxinsertlevel+1)*idx)+i) in
        if prev_node_id = -1 || compare (new_node_arr.(prev_node_id)) (update.(i)) <= 0
        then ((!>node_back).(i) <- update.(i);
              prev_node_idx.(((!maxinsertlevel+1)*idx)+i) <- -2)
      done;
    with Return -> ()

  let remove_duplicates arr num_elements =
    if num_elements <= 1 then num_elements
    else
      let j = ref 0 in
      for i = 0 to num_elements - 2 do
        if arr.(i) <> arr.(i + 1) then (
          arr.(!j) <- arr.(i);
          incr j)
      done;
      arr.(!j) <- arr.(num_elements - 1);
      incr j;
      !j

  let par_insert t (pool : Domainslib.Task.pool) (elems : V.t array) =
    let open Sequential in
    (* Sort in acscending order *)
    Array.sort V.compare elems;
    let num_elems = remove_duplicates elems (Array.length elems) in

    let intermediary = {
      batch_size = num_elems;
      maxinsertlevel = t.level;
      level_arr = Array.make num_elems 0;
      new_node_arr = Array.make num_elems t.nil;
      new_node_back_arr = Array.make num_elems t.nil;
      prev_node_idx = Array.make ((t.maxlevel+1) * num_elems) (-1)
    } in

    for idx = 0 to (num_elems-1) do
      build_node t idx (elems.(idx)) intermediary
    done;

    for idx = 0 to (num_elems-1) do
      relate_nodes t idx intermediary
    done;

    Domainslib.Task.parallel_for pool ~start:0 ~finish:(num_elems-1)
      ~body:(fun idx -> merge_list t idx intermediary);

    for i = 0 to num_elems-1 do
      for j = 0 to intermediary.level_arr.(i) do
        if intermediary.prev_node_idx.(((!(intermediary.maxinsertlevel)+1)*i)+j) = -2
        then begin
          let back_node = (!>(intermediary.new_node_back_arr.(i))).(j) in
          (!>back_node).(j) <- intermediary.new_node_arr.(i)
        end
      done;
    done

  let run t (pool: Domainslib.Task.pool) (ops: wrapped_op array) : unit =
    let inserts: V.t list ref = ref [] in
    let searches: (V.t * (bool -> unit)) list ref = ref [] in
    let size = lazy (Sequential.size t) in
    Array.iter (function
      | Mk (Size, kont) -> kont (Lazy.force size)
      | Mk (Member vl, kont) -> searches := (vl,kont) :: !searches
      | Mk (Insert vl, kont) -> kont (); inserts := vl :: !inserts
    ) ops;
    (* now, do all searches in parallel *)
    let searches = Array.of_list !searches in
    Domainslib.Task.parallel_for pool ~start:0 ~finish:(Array.length searches - 1)
      ~body:(fun i ->
        let key, kont = searches.(i) in
        let result = Sequential.mem t key in
        kont result
      );
    (* now, all inserts *)
    let inserts = Array.of_list !inserts in
    par_insert t pool inserts

end
