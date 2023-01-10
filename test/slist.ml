module T = Domainslib.Task

module type Comparable = sig
  type t
  val compare : t -> t -> int
  val to_string : t -> string
end

module Make (V : Comparable) : sig
  type t

  val make : size:int -> unit -> t
  val search : t -> V.t -> bool
  val insert : t -> V.t -> unit
  val size : t -> int
  val print_slist : t -> unit
  val par_insert : t -> T.pool -> V.t array -> unit
end = struct
  type node =
    | Hd of node array
    | Node of data
    | Null
  and data = {
    mutable value : V.t;
    forward : node array
  }

  let show = function
    | Hd _ -> "Hd"
    | Node {value; _} -> Printf.sprintf "Node(%s)" (V.to_string value)
    | Null -> "Null"

  let[@warning "-32"] to_string = function
    | Hd forward -> "Hd -> [|" ^(Array.fold_right (fun node acc -> acc ^ "; " ^ (show node)) forward "")^"|]"
    | Node {forward;_} as n -> (show n)^"-> [|"^(Array.fold_right (fun node acc -> acc ^ "; " ^ (show node)) forward "")^"|]" 
    | Null -> "Null"

  type t = {
    hdr : node;
    level : int ref;
    maxlevel : int;
    nil : node
  }

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

  let make ~size () =
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

  let search t elt =
    let cnt = ref 0 in
    let x = ref t.hdr in
    for i = !(t.level) downto 0 do
      while
        match (!>(!x)).(i) with
        | Null | Hd _ -> false
        | Node {value ; _} -> value *< elt
      do
        incr cnt;
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

  let print_slist t =
    let print_level t lvl =
      let rec aux = function
        | Null -> print_endline "Null"
        | Hd forward ->
          Printf.printf "Level %d : Hd -> " lvl;
          aux forward.(lvl)
        | Node {value; forward; _} ->
          let val_str = V.to_string value in
          Printf.printf "(%s) -> " val_str;
          aux forward.(lvl)
      in
      aux t.hdr
    in
    for lvl = !(t.level) downto 0 do
      print_level t lvl;
      Printf.printf "\n"
    done

  type intermediate = {
    batch_size : int;
    maxinsertlevel : int ref;
    level_arr : int array;
    new_node_arr : node array;
    new_node_back_arr : node array;
    prev_node_idx : int array
  }

  let build_node t idx elem {maxinsertlevel;
                             new_node_arr;
                             new_node_back_arr;
                             level_arr; _} =
    let rdm_level = random_level t in
    level_arr.(idx) <- rdm_level;
    if rdm_level > !maxinsertlevel then maxinsertlevel := rdm_level;
    let new_node = make_node t rdm_level elem in
    let new_node_back = make_node t rdm_level elem in
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
      (!>node).(lvl) <- t.nil;
      (try 
         for id = !next to (batch_size-1) do
           if lvl <= level_arr.(id) then
             begin
               (* Set forward pointer *)
               (!>node).(lvl) <- new_node_arr.(id);
               (* Set back_pointer *)
               (!>(new_node_back_arr.(id))).(lvl) <- node;
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
    let exception Return in
    let node = new_node_arr.(idx) in
    let node_back = new_node_back_arr.(idx) in
    let update = Array.make (t.maxlevel + 1) t.nil in
    let x = ref t.hdr in
    try 
      for i = !(t.level) downto 0 do
        while
          match (!>(!x)).(i) with
          | Null | Hd _ -> false
          | Node {value ; _} -> value *< (!^node).value
        do
          x := !>(!x).(i)
        done;
        (* No duplicates *)
        (match !x with
         | Null | Hd _ -> ()
         | Node {forward;_} ->
           if forward.(i) != t.nil &&
              (!^(forward.(i))).value *< (!^node).value
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
    
  let par_insert t (pool : T.pool) (elems : V.t array) =
    (* Sort in acscending order *)
    Array.sort V.compare elems;
    let num_elems = Array.length elems in

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

    T.parallel_for pool (*~chunk_size:1*) ~start:0 ~finish:(num_elems-1)
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
end

let test_seq_consistency () =
  let size = 10_000 in
  let module IS = Make (Int) in
  (* Test sequential insert *)
  let t1 = IS.make ~size () in
  for i = 1 to (size/2) do
    IS.insert t1 (i*2)
  done;
  for i = 1 to (size/2) do
    IS.insert t1 ((i-1)*2+1)
  done;
  (* IS.print_slist t1; *)
  assert(IS.search t1 1);
  assert(IS.search t1 size);
  assert(IS.size t1 = size);

  (* Test par_insert *)
  let pool = T.setup_pool ~num_domains:0 () in
  let t2 = IS.make ~size () in
  let elems_even = Array.init (size/2) (fun i -> (i+1)*2) in
  let elems_odd = Array.init (size/2) (fun i -> ((i+1)*2)-1) in
  let main () =
    IS.par_insert t2 pool elems_even;
    assert(IS.size t2 = size/2);
    IS.par_insert t2 pool elems_odd;
    assert(IS.size t2 = size);
    assert(IS.search t2 1);
    assert(IS.search t2 size);
    (* IS.print_slist t2 *)
  in
  T.run pool main;
  T.teardown_pool pool

let test_batch_insert () =
  Format.printf "@." ;
  Format.printf "num_domains: " ;
  for i = 1 to 8 do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  Format.printf "Batch_ins: " ;
  let module ISL = Make (Int) in
  let preset_size = 1_000_000 in 
  let additional = 100_000 in
  let total_size = preset_size + additional in
  let max_rdm_int = (Int.shift_left 1 30) - 1 in 
  let preset_arr =
    Random.init 0;
    Array.init preset_size (fun _ -> Random.int max_rdm_int) in
  let additional_arr =
    Array.init additional (fun _ -> Random.int max_rdm_int) in
  for num_domains = 0 to 7 do
    let t = ISL.make ~size:total_size () in
    let pool = T.setup_pool ~num_domains () in
    Array.iter (fun elt -> ISL.insert t elt) preset_arr;
    Gc.full_major ();
    let t0 = Unix.gettimeofday () in
    T.run pool (fun () -> ISL.par_insert t pool additional_arr);
    let t1 = Unix.gettimeofday () in
    let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
    Format.printf "  %7s%!" (Printf.sprintf "%.0f" op_ms);
    T.teardown_pool pool
  done

let () = test_batch_insert ()
