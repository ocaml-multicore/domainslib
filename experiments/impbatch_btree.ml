open Domainslib
module T = Domainslib.Task

module type V = sig
  type t
end

module BatchedBtree = struct

  type t = string Btree.t 
  type _ batch_op = 
    | Search : int -> string option batch_op
    | Insert : int * string -> unit batch_op

  type wrapped_batch_op = 
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let create () = Btree.create ()

  let search_list = ref []

  let bop : string Btree.t -> T.pool -> wrapped_batch_op array -> int -> unit = 
    fun t pool bop_arr n ->
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, _) -> 
        search_list := key :: !search_list;
      | Batched_op (Insert (key, value), set) -> 
        Btree.insert t key value; set ();
    done;
    let op_arr = Btree.par_search ~pool t (Array.of_list !search_list) in
    let i = ref 0 in
    Array.iter (fun (res : 'a option) -> 
        while match bop_arr.(!i) with 
            Batched_op (Search _, _) -> false 
          | _ -> true do
          incr i
        done;
        match bop_arr.(!i) with 
        | Batched_op (Search _, set) -> set res 
        | _ -> failwith "Impossible") op_arr
end
(* 
module PolyBatchedBtree = struct

  type 'elt t = 'elt Btree.t 
  type ('elt, _) batch_op = 
    | Search : int -> ('elt, 'elt option) batch_op
    | Insert : int * 'elt -> ('elt, unit) batch_op

  type wrapped_batch_op = 
      Batched_op : ('elt, 'a) batch_op * ('a -> unit) -> wrapped_batch_op

  let create () = Btree.create ()

  let search_list = ref []

  let bop : 'a t -> T.pool -> wrapped_batch_op array -> int -> unit = 
    fun t pool bop_arr n ->
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, _) -> 
        search_list := key :: !search_list;
      | Batched_op (Insert (key, value), set) -> 
        Btree.insert t key value; set ();
    done;
    let op_arr = Btree.par_search ~pool t (Array.of_list !search_list) in
    let i = ref 0 in
    Array.iter (fun (res : 'a option) -> 
        while match bop_arr.(!i) with 
            Batched_op (Search _, _) -> false 
          | _ -> true do
          incr i
        done;
        match bop_arr.(!i) with 
        | Batched_op (Search _, set) -> set res 
        | _ -> failwith "Impossible") op_arr
end *)

module ImpBatchedBtree = struct
  include Batcher.Make(BatchedBtree)
  let search t i = batchify t (Search i)
  let insert t k v = batchify t (Insert (k, v))
end

