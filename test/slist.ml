module T = Domainslib.Task

let num_domains = Domain.recommended_domain_count () - 1
let run = try bool_of_string Sys.argv.(1) with _ -> false
let batch_size = 127
let preset_size = 1_000_000
let additional = 1_00_000
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
  val validate : t -> unit
  val print_slist : t -> unit
  val par_insert : t -> T.pool -> V.t array -> unit
  val par_insert_mock : t -> T.pool -> V.t array -> unit
end = struct

  type t = {
    hdr : node;
    level : int ref;
    maxlevel : int;
    nil : node
  } 
  and node = Hd of node array | Node of data | Null
  and data = {mutable value : V.t; forward : node array}

  let show = function
    | Hd _ -> "Hd"
    | Node {value; _} -> Printf.sprintf "Node(%s)" (V.to_string value)
    | Null -> "Null"

  let[@warning "-32"] to_string = function
    | Hd forward -> "Hd -> [|" ^(Array.fold_right (fun node acc -> acc ^ "; " ^ (show node)) forward "")^"|]"
    | Node {forward;_} as n -> (show n)^"-> [|"^(Array.fold_right (fun node acc -> acc ^ "; " ^ (show node)) forward "")^"|]"
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
  let validate t =
    let rec walk prev = function
      | Null -> ()
      | Hd forward -> walk prev forward.(0)
      | Node {value; forward; _} ->
        let vals = value |> V.to_string in
        let prevs = prev |> V.to_string in
        (if value < prev then Printf.printf "Ordering error %s -> %s\n" vals prevs
         else if value = prev then Printf.printf "Duplicate error %s -> %s\n" vals prevs);
        walk value forward.(0)
    in
    let starting_point =  !>(t.hdr).(0) in
    let first_val = !^starting_point.value in
    walk first_val !>starting_point.(0)

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

  let[@warning "-32"] par_insert t (pool : T.pool) (elems : V.t array) =
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

    T.parallel_for pool ~start:0 ~finish:(num_elems-1)
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

  let par_insert_mock _t (pool : T.pool) (elems : V.t array) =
    let num_per_elem = 500 in
    let num_elems = Array.length elems in
    let size = num_per_elem * num_elems in

    let g_touched = Array.init size (fun _ -> 0) in

    T.parallel_for pool ~start:0 ~finish:(size - 1) ~body:
      (fun i -> g_touched.(i) <- -1);

    Array.iter (fun elt -> assert(elt = -1)) g_touched

end

let stats = Hashtbl.create 100
module MakeImpBatched (V : Comparable) : sig
  type t

  val make : size:int -> batch_size:int -> unit -> t
  val search : t -> V.t -> bool
  val size : t -> int
  val seq_ins : t -> V.t -> unit
  val batch_ins : t -> T.pool -> V.t array -> unit
  val imp_batch_ins : t -> T.pool -> V.t -> unit
end = struct
  module SL = Make(V)
  module Q = Mpmc_queue
  type t = {
    slist : SL.t;
    batch_size : int Atomic.t;
    running : bool Atomic.t;
    q : batch_op Q.t;
    container : batch_op array;
  }
  and
    batch_op =
    | Ins of t * V.t * (unit -> unit)
    | Null

  let make ~size ~batch_size() =
    {slist = SL.make ~size ();
     batch_size = Atomic.make 0;
     running = Atomic.make false;
     q = Q.make ();
     container = Array.make batch_size Null;
    }

  let rec try_launch pool t =
    if Atomic.compare_and_set t.running false true then
      match Q.pop t.q with
      | Some op -> t.container.(0) <- op;
        (let i = ref 1 in
         while
           match Q.pop t.q with
           | Some op -> t.container.(!i) <- op; incr i; true
           | None -> false
         do () done;
         (match Hashtbl.find_opt stats !i with
          | Some cnt -> Hashtbl.replace stats !i (cnt + 1)
          | None -> Hashtbl.add stats !i 1);
         let batch = Array.init !i (fun i -> t.container.(i)) in
         (match Hashtbl.find_opt stats !i with
          | Some cnt -> Hashtbl.replace stats !i (cnt + 1)
          | None -> Hashtbl.add stats !i 1);
         let data = Array.mapi (fun _ op ->
             match op with
             | Ins (_, elt, set) -> set (); elt
             | Null -> failwith "Error") batch in
         SL.par_insert t.slist pool data;
         Atomic.set t.running false;
         try_launch pool t)
      | None -> Atomic.set t.running false

  let seq_ins t elt = SL.insert t.slist elt

  let batch_ins t pool elt_arr = SL.par_insert t.slist pool elt_arr

  let imp_batch_ins t pool elt =
    let pr, set = T.promise () in
    Q.push t.q (Ins (t, elt, set));
    try_launch pool t;
    T.await pool pr
  let search t = SL.search t.slist
  let size t = SL.size t.slist
end

let test_correctness () =
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
  let pool = T.setup_pool ~num_domains:7 () in
  let t2 = IS.make ~size () in
  let elems_even = Array.init (size/2) (fun i -> (i+1)*2) in
  let elems_odd = Array.init (size/2) (fun i -> ((i+1)*2)-1) in
  let test_par_insert () =
    IS.par_insert t2 pool elems_even;
    assert(IS.size t2 = size/2);
    IS.par_insert t2 pool elems_odd;
    assert(IS.size t2 = size);
    assert(IS.search t2 1);
    assert(IS.search t2 size);
    (* IS.print_slist t2 *)
  in
  T.run pool test_par_insert;

  (* Test uniqueness invariant *)
  let t3 = IS.make ~size:100 () in
  let preset = [|5;10;56;100|] in
  Array.iter (fun elt -> IS.insert t3 elt) preset;
  assert(IS.search t3 5);
  let additional = [|5;6;5;10;9;101;100;55;56|] in
  let test_uniqueness () =
    IS.par_insert t3 pool additional;
    assert(IS.size t3 = 8);
    assert(IS.search t3 56);
  in
  T.run pool test_uniqueness;

  (* Stress test inserts *)
  let preset = 1_000_000 in
  let additional = 100_000 in
  let size = preset + additional in
  let t4 = IS.make ~size () in
  Random.init 0;
  let max_rdm_int = (Int.shift_left 1 30) - 1 in
  let random_preset = Array.init preset (fun _ -> Random.int max_rdm_int) in
  let random_additional = Array.init additional (fun _ -> Random.int max_rdm_int) in
  Array.iter (fun elt -> IS.insert t4 elt) random_preset;
  let batch_insert () = IS.par_insert t4 pool random_additional in
  T.run pool batch_insert;
  let total_arr = Array.append random_preset random_additional in
  Array.sort Int.compare total_arr;
  let count_unique arr =
    let len = Array.length arr in
    let rec aux idx acc =
      if idx >= len then acc else
        begin
          if arr.(idx-1) = arr.(idx) then aux (idx+1) acc else aux (idx+1) (acc+1)
        end
    in
    if len = 0 then 0 else aux 1 1
  in
  IS.validate t4;
  assert(count_unique total_arr = IS.size t4);
  T.teardown_pool pool

let run_stats () =
  let module ISL = MakeImpBatched (Int) in
  let total_size = preset_size + additional in
  let max_rdm_int = (Int.shift_left 1 30) - 1 in
  let preset_arr = Random.init 0;
    Array.init preset_size (fun _ -> Random.int max_rdm_int) in
  let additional_arr =
    Array.init additional (fun _ -> Random.int max_rdm_int) in
  let t = ISL.make ~size:total_size ~batch_size () in
  let pool = T.setup_pool ~num_domains () in
  Array.iter (fun elt -> ISL.seq_ins t elt) preset_arr;
  let chunk_size = 
    Util.chunk_calculator ~batch_size ~operations:additional () in
  T.run pool (fun () -> 
      T.parallel_for pool ~chunk_size ~start:0 ~finish:(additional-1) 
        ~body:(fun i -> ISL.imp_batch_ins t pool additional_arr.(i)));
  Util.print_implicit_batch_stats stats;
  T.teardown_pool pool

let () =   
  if run then 
    begin 
      Printf.printf "\nRunning ImpBatchSlist Statistics, batch_size = %d, preset = %d, additional inserts = %d\n%!" batch_size preset_size additional;
      run_stats ()
    end;
