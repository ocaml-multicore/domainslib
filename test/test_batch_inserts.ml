module T = Domainslib.Task
module Batched_slist = functor (V : Slist.Comparable) -> struct
  module SL = Slist.Make(V)
  module Q = Mpmc_queue
  type t = {
    slist : SL.t;
    batch_size : int Atomic.t;
    running : bool Atomic.t;
    q : batch_op Q.t;
    container : batch_op array;
    stats : (int, int) Hashtbl.t
  }
  and
    batch_op =
    | Ins of t * V.t * (unit -> unit)
    | Null

  let make ~size () =
    {slist = SL.make ~size ();
     batch_size = Atomic.make 0;
     running = Atomic.make false;
     q = Q.make ();
     container = Array.make size Null;
     stats = Hashtbl.create 100;
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
         let batch = Array.init !i (fun i -> t.container.(i)) in
         let data = Array.mapi (fun i op ->
             match op with
             | Ins (_, elt, set) -> if i > 0 then set (); elt
             | Null -> failwith "Error") batch in
         SL.par_insert t.slist pool data;
         (match batch.(0) with Ins (_,_,set) -> set () | _ -> failwith "Bad");
         (* (match Hashtbl.find_opt t.stats !i with *)
         (*  | Some cnt -> Hashtbl.replace t.stats !i (cnt + 1) *)
         (*  | None -> Hashtbl.add t.stats !i 1); *)
         Atomic.set t.running false;
         try_launch pool t)
      | None -> Atomic.set t.running false

  let seq_ins t elt = SL.insert t.slist elt

  let imp_batch_ins t pool elt =
    let pr, set = T.promise () in
    Q.push t.q (Ins (t, elt, set));
    try_launch pool t;
    T.await pool pr

  let batch_ins t pool arr = SL.par_insert t.slist pool arr

  
  let search t = SL.search t.slist
  let size t = SL.size t.slist
  let print_stats t = Hashtbl.iter (fun key value -> Printf.printf "key(%d) -> %d\n" key value) t.stats
end
module ISL = Batched_slist(Int)

let max_rdm_int = (Int.shift_left 1 30) - 1
let usage_msg = "-num_domains <7> -preset <1_000_000> -inserts <100_000>"
let num_domains = ref (Domain.recommended_domain_count () - 1)
let preset = ref 1_000_000
let inserts = ref 100_000
let total_size = ref (!preset + !inserts)
let speclist =
  [
    ("-num_domains", Arg.Set_int num_domains, "Set number of additional domains");
    ("-preset", Arg.Set_int preset, "Set number of preset elements");
    ("-inserts", Arg.Set_int inserts, "Set number of inserts")
  ]

let init () =
  let t = ISL.make ~size:(!preset + !inserts) () in
  for _ = 1 to !preset do
    (* Insert Random *)
    let rdm = Random.int max_rdm_int in
    ISL.seq_ins t rdm
  done;
  t

let with_pool f t num_domains insert_arr =
  let pool = T.setup_pool ~num_domains () in
  T.run pool (f pool t insert_arr) ;
  T.teardown_pool pool

let test_batch pool t insert_arr () = ISL.batch_ins t pool insert_arr

let test_imp_batching pool t insert_arr () = T.parallel_for pool ~start:0 ~finish:(!inserts-1) ~body:(fun i ->
    ISL.imp_batch_ins t pool (insert_arr.(i)))
 
let run_iter _pool n =
  let insert_arr = Array.init !inserts (fun _ -> Random.int max_rdm_int) in
  for _ = 1 to n do
    let t = init () in
    Gc.full_major ();
    let t0 = Unix.gettimeofday () in
    with_pool (test_batch) t !num_domains insert_arr;
    let t1 = Unix.gettimeofday () in
    let op_ms = (Int.to_float !inserts) /. (1000.0 *. (t1 -. t0)) in
    Printf.printf "%.0f\n%!" op_ms
  done

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  total_size := !preset + !inserts;
  Printf.printf "Run %s on num_domains=%d, preset=%d, inserts=%d\n"
    Sys.executable_name !num_domains !preset !inserts;
  let pool = T.setup_pool ~num_domains:(!num_domains) () in
  run_iter pool 1;
  T.teardown_pool pool
        
