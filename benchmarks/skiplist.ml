module IntSkiplist = Data.Skiplist.Make(Int)
module BatchedSkiplist = Domainslib.Batcher.Make(Data.Skiplist.Make(Int))

type generic_test_spec = {
  initial_elements: int array;
  insert_elements : int array;
  search_elements : int array;
  size : int
}

type generic_spec_args = {
  sorted : bool;
  no_searches : int;
  no_size : int
}

let generic_spec_args: generic_spec_args Cmdliner.Term.t =
  let open Cmdliner in
  let sorted = Arg.(value @@ flag  @@ info ~doc:"whether the inserts should be sorted" ["s"; "sorted"]) in
  let no_searches =
    Arg.(value @@ opt (some int) None @@ info ~doc:"number of searches" ~docv:"NO_SEARCHES" ["n"; "no-searches"]) in
  let no_size = 
    Arg.(value @@ opt (some int) None @@ info ~doc:"number of size operation calls" ~docv:"NO_SIZE" ["n_sz"; "no-size"]) in
  Term.(const (fun sorted no_searches no_size -> {sorted; no_searches=Option.value ~default:0 no_searches; no_size=Option.value ~default:0 no_size}) $ sorted $ no_searches $ no_size)

let generic_test_spec ~initial_count ~count ~min ~max spec_args =
  let all_elements = Util.gen_random_array ~min ~max (initial_count + count) in
  let search_elements = Util.gen_random_array ~min ~max spec_args.no_searches in
  let initial_elements = Array.make initial_count min in
  let insert_elements = Array.make count min in
  Array.blit
    all_elements 0
    initial_elements 0
    initial_count;
  Array.blit
    all_elements initial_count
    insert_elements 0
    count;
  if spec_args.sorted then
    Array.sort Int.compare insert_elements;
  { initial_elements; insert_elements; search_elements; size=spec_args.no_size }

module Sequential = struct

  type t = IntSkiplist.Sequential.t

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~initial_count ~count ~min ~max spec_args =
    generic_test_spec ~initial_count ~count ~min ~max spec_args

  let init _pool test_spec = 
    let size = Array.length test_spec.initial_elements + Array.length test_spec.insert_elements in
    let skiplist = IntSkiplist.Sequential.init ~size () in
    Array.iter (fun i -> IntSkiplist.Sequential.insert skiplist i) test_spec.initial_elements;
    skiplist

  let run _pool t test_spec =
    Array.iter (fun i ->
        IntSkiplist.Sequential.insert t i) test_spec.insert_elements;
    Array.iter (fun i ->
        IntSkiplist.Sequential.mem t i |> ignore) test_spec.search_elements;
    for _ = 1 to test_spec.size do
      IntSkiplist.Sequential.size t |> ignore
    done
end


module CoarseGrained = struct

  type t = {skiplist : IntSkiplist.Sequential.t; mutex : Mutex.t}

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args


  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~initial_count ~count ~min ~max spec_args =
    generic_test_spec ~initial_count ~count ~min ~max spec_args

  let init _pool test_spec = 
    let size = Array.length test_spec.initial_elements + Array.length test_spec.insert_elements in
    let skiplist = IntSkiplist.Sequential.init ~size () in
    Array.iter (fun i -> IntSkiplist.Sequential.insert skiplist i) test_spec.initial_elements;
    {skiplist; mutex=Mutex.create ()}

  let run pool t test_spec =
    let total = Array.length test_spec.insert_elements + Array.length test_spec.search_elements + test_spec.size - 1 in
    Domainslib.Task.parallel_for pool
      ~start:0 ~finish:total
      ~body:(fun i ->
          Mutex.lock t.mutex;
          Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
              if i < Array.length test_spec.insert_elements
              then IntSkiplist.Sequential.insert t.skiplist test_spec.insert_elements.(i)
              else if i < Array.length test_spec.insert_elements + Array.length test_spec.search_elements then
                ignore (IntSkiplist.Sequential.mem t.skiplist
                          test_spec.search_elements.(i - Array.length test_spec.search_elements))
              else
                ignore (IntSkiplist.Sequential.size t.skiplist)
            )
        )
end

module Batched = struct

  type t = BatchedSkiplist.t

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~initial_count ~count ~min ~max spec_args =
    generic_test_spec ~initial_count ~count ~min ~max spec_args

  let init pool test_spec = 
    let skiplist = BatchedSkiplist.init pool in
    Array.iter (fun i -> BatchedSkiplist.apply skiplist (Insert i)) test_spec.initial_elements;
    skiplist

  let run pool t test_spec =
    let total = Array.length test_spec.insert_elements + Array.length test_spec.search_elements + test_spec.size - 1 in
    Domainslib.Task.parallel_for pool
      ~start:0 ~finish:total
      ~body:(fun i ->
          if i < Array.length test_spec.insert_elements
          then BatchedSkiplist.apply t (Insert test_spec.insert_elements.(i))
          else if i < Array.length test_spec.insert_elements + Array.length test_spec.search_elements then
            ignore (BatchedSkiplist.apply t 
                      (Member test_spec.search_elements.(i - Array.length test_spec.search_elements)))
          else
            ignore (BatchedSkiplist.apply t Size)
        )

end

