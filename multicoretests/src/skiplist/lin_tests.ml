open Lin_batched

module IntSkipList = Data.Skiplist.Make(Int)

(** ********************************************************************** *)
(**                      Tests of batched [skiplist]                       *)
(** ********************************************************************** *)
module Skiplist : Spec = struct


  type t = IntSkipList.Sequential.t
  type wrapped_op = IntSkipList.wrapped_op

  let init : unit -> t = IntSkipList.init

  let cleanup : t -> unit = fun _ -> ()

  let run : Domainslib.Task.pool -> t -> wrapped_op list -> unit =
    fun pool state ops ->
    let ops = Array.of_list ops in
    IntSkipList.run state pool ops
    
  let api : (int * (t, wrapped_op) elem) list = [
    val_ "insert"
      IntSkipList.Sequential.insert
      (fun set _ vl -> IntSkipList.(Mk (Insert vl, set)))
      (t @-> int @-> returning unit);
    val_ "member"
      IntSkipList.Sequential.mem
      (fun set _ vl -> IntSkipList.(Mk (Member vl, set)))
      (t @-> int @-> returning bool);
    val_ "size"
      IntSkipList.Sequential.size
      (fun set _ -> IntSkipList.(Mk (Size, set)))
      (t @-> returning int);
  ]

end

module SkiplistTest = Lin_batched.Make(Skiplist) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
  SkiplistTest.lin_test ~count:100 ~name:"Skiplist's Batched API is linearisable"
]
