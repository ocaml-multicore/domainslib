module T = Domainslib.Task
module type Slist =
  functor (V: Batched_slist.Comparable) ->
  sig
    type t

    val make : size:int -> unit -> t
    val search : t -> V.t -> bool
    val seq_ins : t -> V.t -> unit
    val batch_ins : T.pool -> t -> V.t -> unit
    val size : t -> int
    val print_stats : t -> unit
  end

module Bench (SLF : Slist) = struct
  module SL = SLF(Int)

  module T = Domainslib.Task
  let preset_size = 10_000_000
  let additional = 100_000
  let total_size = preset_size + additional
  let additional_arr = Array.make additional 0
  let max_rdm_int = (Int.shift_left 1 30) - 1
                   
  let init () =
    Random.init 0;
    let t = SL.make ~size:total_size () in
    for _ = 1 to preset_size do
      (* Insert Random *)
      let rdm = Random.int max_rdm_int in
      SL.seq_ins t rdm
    done;
    for i = 0 to (additional-1) do
      additional_arr.(i) <-  Random.int max_rdm_int
    done;
    t

  let with_pool f t num_domains =
    let pool = T.setup_pool ~num_domains () in
    T.run pool (f pool t) ;
    T.teardown_pool pool    

  let test_seq _pool t () =
    Array.iter (fun elt -> SL.seq_ins t elt) additional_arr

  let run_seq () =
    for i = 1 to 7 do                    
      let t = init () in
      let t0 = Unix.gettimeofday () in
      with_pool (test_seq) t i;
      let t1 = Unix.gettimeofday () in
      let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
      Format.printf "  %7s%!"
        (Printf.sprintf "%.0f" op_ms);
      (* Caml.Format.printf "  %7s%!" *)
      (*   (Printf.sprintf "%.0f" (1000.0 *. (t1 -. t0))) *)
    done ;
    Caml.Format.printf "@."

  let test_batch pool t () =
    T.parallel_for pool ~start:0 ~finish:(additional-1) ~body:(fun i ->
        SL.batch_ins pool t (additional_arr.(i))
      )
    (* SL.print_stats t *)
    (* Printf.printf "Size = %d\n" (SL.size t); *)
    (* assert (SL.size t = total_size) *)

  let run_batch () =
    for i = 1 to 7 do                    
      let t = init () in
      let t0 = Unix.gettimeofday () in
      with_pool (test_batch) t i;
      let t1 = Unix.gettimeofday () in
      let op_ms = (Int.to_float additional) /. (1000.0 *. (t1 -. t0)) in
      Format.printf "  %7s%!"
        (Printf.sprintf "%.0f" op_ms);
      (* Format.printf "  %7s%!" *)
      (*   (Printf.sprintf "%.2f ms" (1000.0 *. (t1 -. t0))) *)
    done ;
    Format.printf "@."
end

module Batched_slist : Slist = functor (V : Batched_slist.Comparable) -> struct
  module SL = Batched_slist.Make(V)
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
         (match Hashtbl.find_opt t.stats !i with
          | Some cnt -> Hashtbl.replace t.stats !i (cnt + 1)
          | None -> Hashtbl.add t.stats !i 1);
         Atomic.set t.running false;
         try_launch pool t)
      | None -> Atomic.set t.running false

  let seq_ins t elt = SL.insert t.slist elt

  let batch_ins pool t elt =
    let pr, set = T.promise () in
    Q.push t.q (Ins (t, elt, set));
    try_launch pool t;
    T.await pool pr

  let search t = SL.search t.slist
  let size t = SL.size t.slist
  let print_stats t = Hashtbl.iter (fun key value -> Printf.printf "key(%d) -> %d\n" key value) t.stats
end

module Bench_batched_slist =  Bench (Batched_slist)

let () =
  Format.printf "@." ;
  Format.printf "  num_domains: " ;
  for i = 1 to 7 do
    Format.printf " %5i   " i
  done ;
  Format.printf "@." ;
  Caml.Format.printf "    Seq_ins:  " ;
  Bench_batched_slist.run_seq () ;
  Format.printf "Batched_ins:  " ;
  Bench_batched_slist.run_batch () ;


