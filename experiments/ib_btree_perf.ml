module T = Domainslib.Task

let num_domains = 7
let pool = T.setup_pool ~num_domains ()
let max_rdm_int = (Int.shift_left 1 30) - 1
let size = 10_000_000
let elems = Array.init size (fun _ ->
    let rdm = Random.int max_rdm_int in
    rdm, "key" ^ string_of_int rdm)

let seq_insert () =
  let t = Btree.create () in
  Array.iter (fun (k,v) -> Btree.insert t k v) elems

module type IBSig = sig
  type t
  val create : T.pool -> t
  val insert : t -> int -> string -> unit
end

module IB_Btree_tester(Btree : IBSig) = struct 

  let par_insert pool () =
    let t = Btree.create pool in
    T.parallel_for pool ~chunk_size:(size) ~start:0 ~finish:(size-1) 
      ~body:(fun i -> let k,v = elems.(i) in Btree.insert t k v)

end

(* module IB_Btree_Seq = IB_Btree_tester(Ib_btree.Seq.ImpBatchedBtree)
   module IB_Btree_Par = IB_Btree_tester(Ib_btree.Par.ImpBatchedBtree) *)


let () =
  Format.printf "\n@[Sequential build: %.10fs@]@."
    (Utils.time seq_insert);
  (* Format.printf "@[Batch rebuild: %.10fs@]@."
     (Utils.time (par_insert pool)) *)