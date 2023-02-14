open Lin_batched

(** ********************************************************************** *)
(**                      Tests of thread-unsafe [Array]                    *)
(** ********************************************************************** *)
module Counter : Spec = struct

  type t = Data.Counter.t
  type wrapped_op = Data.Counter.wrapped_op

  let init : unit -> t = Data.Counter.init

  let cleanup : t -> unit = fun _ -> ()

  let run : Domainslib.Task.pool -> t -> wrapped_op list -> unit =
    fun pool state ops ->
    let ops = Array.of_list ops in
    Data.Counter.run state pool ops
    
  let api : (int * (t, wrapped_op) elem) list = [
    val_ "incr"
      Data.Counter.Sequential.incr
      (fun set _ -> Data.Counter.(Mk (Incr, set)))
      (t @-> returning unit);
    val_ "decr"
      Data.Counter.Sequential.decr
      (fun set _ -> Data.Counter.(Mk (Decr, set)))
      (t @-> returning unit);
    val_ "get"
      Data.Counter.Sequential.get
      (fun set _ -> Data.Counter.(Mk (Get, set)))
      (t @-> returning int);
  ]

end

module CounterTest = Lin_batched.Make(Counter) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
  CounterTest.lin_test ~count:10 ~name:"Counter's Batched API is linearisable"
]
