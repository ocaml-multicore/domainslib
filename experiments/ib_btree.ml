open Domainslib
module T = Domainslib.Task
module Btree = Batch_para_btree

module type V = sig
  type t
  val empty : t
end

(* Define Explicitly Batched version of data structure *)
module ExBatchedBtree(V : V) = struct

  type t = V.t Btree.t 
  type _ batch_op = 
    | Search : int -> V.t option batch_op
    | Insert : int * V.t -> unit batch_op

  type wrapped_batch_op = 
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let create () = Btree.create ()
  let sz = 1_000_000
  let batch_insert = Array.make sz (0, V.empty)

  let bop : V.t Btree.t -> Task.pool -> wrapped_batch_op array -> int -> unit = 
    fun t pool bop_arr n ->
    assert (n < sz);
    let idx = ref 0 in
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, set) -> 
        set @@ Btree.search t key
      | Batched_op (Insert (key, value), set) -> 
        set @@ Btree.insert t key value;
        batch_insert.(!idx) <- (key, value);
        incr idx;
    done;
    Batch_para_btree.par_insert_rebuilder ~pool t batch_insert
end

include Batcher.Make(ExBatchedBtree(String))
let _search t i = batchify t (Search i)
let insert t k v = batchify t (Insert (k, v))

let num_domains = 7
let max_rdm_int = (Int.shift_left 1 30) - 1
let size = 100_000
let elems = Array.init size (fun _ -> 
    let rdm = Random.int max_rdm_int in
    rdm, "key" ^ string_of_int rdm)

let[@warning "-32"] seq_insert _pool () =
  let t = Btree.create () in
  Array.iter (fun (k,v) -> Btree.insert t k v) elems

let par_insert pool () = 
  let t = create pool in
  T.parallel_for pool ~start:0 ~finish:(size-1) ~body:(fun i ->
      let k,v = elems.(i) in
      insert t k v)

let run f num_domains =
  let pool = T.setup_pool ~num_domains () in
  let t0 = Unix.gettimeofday () in
  let _tree = T.run pool (f pool) in
  let t1 = Unix.gettimeofday () in
  let time = (t1 -. t0) in
  T.teardown_pool pool;
  time

let () = 
  Format.printf "\n@[Sequential build: %.10fs@]@."
    (run seq_insert num_domains);
  Gc.full_major ();
  Format.printf "@[Batch rebuild: %.10fs@]@." 
    (run par_insert num_domains)