module type S = sig
  type elt
  type t
  val empty    : unit -> t
  val mem      : elt -> t -> bool
  val add      : elt -> t -> unit
  val cardinal : t -> int
  val remove   : elt -> t -> elt option
end

module Lib : sig
  module Make : functor (Ord : Set.OrderedType) -> S with type elt = Ord.t
end
= struct
  module Make (Ord : Set.OrderedType) =
    struct
    module S = Set.Make (Ord)

    type elt = Ord.t

    type t = {
      mutable content  : S.t;
      mutable cardinal : int;
      mutex            : Mutex.t}

    let empty () = { content = S.empty; cardinal = 0; mutex = Mutex.create () }

    let mem_non_lock a t = S.mem a t.content

    let mem a t =
      Mutex.lock t.mutex;
      let b = mem_non_lock a t in
      Mutex.unlock t.mutex;
      b

    let add a t =
      Mutex.lock t.mutex;
      if not (mem_non_lock a t)
      then begin
        t.content <- S.add a t.content;
        t.cardinal <- t.cardinal + 1;
      end;
      Mutex.unlock t.mutex

    let cardinal t =
      Mutex.lock t.mutex;
      let c = t.cardinal in
      Mutex.unlock t.mutex;
      c

    let remove a t =
      Mutex.lock t.mutex;
      let r =
        if mem_non_lock a t
        then begin
          t.content  <- S.remove a t.content;
          (* t.cardinal <- t.cardinal - 1; *)
          Some a
        end
        else None
      in
      Mutex.unlock t.mutex;
      r
  end
end

open QCheck
open STM

module Lib_spec : Spec = struct

  module S = Lib.Make (Int)

  type sut = S.t
  let init_sut () = S.empty ()
  let cleanup _ = ()

  type cmd =
    | Mem of int
    | Add of int
    | Cardinal
    | Remove of int [@@deriving show { with_path = false }]

  let run cmd sut =
    match cmd with
    | Mem i    -> Res (bool, S.mem i sut)
    | Add i    -> Res (unit, S.add i sut)
    | Cardinal -> Res (int, S.cardinal sut)
    | Remove i -> Res (option int, S.remove i sut)

  type state = int list
  let init_state = []

  let next_state cmd state =
    match cmd with
    | Mem _    -> state
    | Add i    -> if List.mem i state then state else i :: state
    | Cardinal -> state
    | Remove i -> if List.mem i state then List.filter (fun x -> x <> i) state else state

  let precond _cmd _state = true

  let postcond cmd state res =
    match cmd, res with
    | Mem i,  Res ((Bool,_), b)               -> b = List.mem i state
    | Cardinal, Res ((Int,_), l)              -> l = List.length state
    | Add _,  Res ((Unit,_),_)                -> true
    | Remove i, Res ((Option Int, _), Some x) -> List.mem i state && i = x
    | Remove i, Res ((Option Int, _), None)   -> not (List.mem i state)
    | _                                       -> false

  let arb_cmd state =
    let gen =
      match state with
      | [] -> Gen.int
      | xs -> Gen.(oneof [oneofl xs; int])
    in
    QCheck.make ~print:show_cmd
      (QCheck.Gen.oneof
        [Gen.return Cardinal;
         Gen.map (fun i -> Mem i) gen;
         Gen.map (fun i -> Add i) gen;
         Gen.map (fun i -> Remove i) gen;
        ])
end

module Lib_sequential = STM_sequential.Make(Lib_spec)

let _ = QCheck_base_runner.run_tests_main
    [Lib_sequential.agree_test ~count:100 ~name:"STM sequential tests"]
