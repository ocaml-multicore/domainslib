open Domainslib
module T = Domainslib.Task

module type V = sig
  type t
end
module BatchedBtree = struct

  type t = {
    pool : Task.pool;
    mutable ds : string Btree.t }

  type _ batch_op = 
    | Search : int -> string option batch_op
    | Insert : int * string -> unit batch_op

  type wrapped_batch_op = 
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let unload t = t.ds
  let load t ds = t.ds <- ds

  let create pool = {
    pool;
    ds = Btree.create ()
  }

  let search_list = ref []
  let print_tree t =
    print_endline (Btree.show (fun fmt vl -> Format.fprintf fmt "\"%s\"" vl) t.ds)

  let bop : t -> wrapped_batch_op array -> int -> unit = fun t bop_arr n ->
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, _) -> 
        search_list := key :: !search_list;
      | Batched_op (Insert (key, value), set) -> 
        Btree.insert t.ds key value; set ();
    done;
    let op_arr = Btree.par_search ~pool:t.pool t.ds (Array.of_list !search_list) in
    let i = ref 0 in
    Array.iter (fun (res : string option) -> 
        while match bop_arr.(!i) with 
            Batched_op (Search _, _) -> false 
          | _ -> true do
          incr i
        done;
        match bop_arr.(!i) with 
        | Batched_op (Search _, set) -> set res 
        | _ -> failwith "Impossible") op_arr
end

module ImpBatchedBtree = struct
  include BatchedBtree
  include Batcher.Make2(BatchedBtree)

  let create = create
  let search t i = batchify t (Search i)
  let insert t k v = batchify t (Insert (k, v))
end

