open Domainslib
module T = Domainslib.Task

module BatchedBtree (V : sig
    type t
  end) : Batcher.BatchedDS = struct
  type t = {
    pool : Task.pool;
    ds : V.t Btree.t }
  type _ batch_op = 
    | Search : int -> V.t option batch_op
    | Insert : int * V.t -> unit batch_op

  type wrapped_batch_op = 
      Batched_op : 'a batch_op * ('a -> unit) -> wrapped_batch_op

  let create pool ={
    pool;
    ds = Btree.create ()
  }

  let search_arr = ref []
  let search_setters = ref []

  let bop : t -> wrapped_batch_op array -> int -> unit = fun t bop_arr n ->
    for i = 0 to n-1 do
      match bop_arr.(i) with
      | Batched_op (Search key, _) -> search_arr := key :: !search_arr;
      | Batched_op (Insert (key, value), set) -> failwith "Dont know what to do"
    done;
    let op_arr = Btree.par_search ~pool:t.pool t.ds (Array.of_list !search_arr) in
    let i = ref 0 in
    Array.iter (fun (res : V.t option) -> 
        while match bop_arr.(!i) with 
            Batched_op (Search _, _) -> false 
          | _ -> true do
          incr i
        done;
        match bop_arr.(!i) with 
        | Batched_op (Search _, set) -> set res 
        | _ -> failwith "Impossible") op_arr

end