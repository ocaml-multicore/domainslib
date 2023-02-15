open Lin_batched

module IntBtree = Data.Btree.Make(Int)

(** ********************************************************************** *)
(**                      Tests of batched [skiplist]                       *)
(** ********************************************************************** *)
module Btree : Spec = struct


  type t = int IntBtree.Sequential.t
  type wrapped_op = int IntBtree.wrapped_op

  let init : unit -> t = IntBtree.init

  let cleanup : t -> unit = fun _ -> ()

  let run : Domainslib.Task.pool -> t -> wrapped_op list -> unit =
    fun pool state ops ->
    let ops = Array.of_list ops in
    IntBtree.run state pool ops
    
  let api : (int * (t, wrapped_op) elem) list = [
    val_ "insert"
      IntBtree.Sequential.insert
      (fun set _ key vl -> IntBtree.(Mk (Insert (key,vl), set)))
      (t @-> int @-> int @-> returning unit);
    val_ "search"
      IntBtree.Sequential.search
      (fun set _ key -> IntBtree.(Mk (Search key, set)))
      (t @-> int @-> returning (option int));
    val_ "size"
      IntBtree.Sequential.size
      (fun set _ -> IntBtree.(Mk (Size, set)))
      (t @-> returning int);
  ]

end

module BtreeTest = Lin_batched.Make(Btree) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
  BtreeTest.lin_test ~count:100 ~name:"Btree's Batched API is linearisable"
]
