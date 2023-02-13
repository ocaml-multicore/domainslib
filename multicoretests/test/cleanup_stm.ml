open QCheck
open STM

exception Cleanup_without_init
exception Already_cleaned
exception Random_postcond_failure

type status = Inited | Cleaned

let status = ref None (* global ref to keep track of cleanup/init status *)

(** This is a variant of refs to test for missing and double cleanup *)

module RConf =
struct

  type cmd =
    | Get
    | Set of int
    | Add of int [@@deriving show { with_path = false }]

  let gen_cmd =
    let int_gen = Gen.nat in
      (Gen.oneof
         [Gen.return Get;
	  Gen.map (fun i -> Set i) int_gen;
	  Gen.map (fun i -> Add i) int_gen;
         ])
  let arb_cmd _ = make ~print:show_cmd gen_cmd

  type state = int

  let init_state = 0

  let next_state c s = match c with
    | Get -> s
    | Set i -> i
    | Add i -> s+i

  type sut = int ref

  let init_sut () =
    assert (!status = None || !status = Some Cleaned);
    status := Some Inited;
    ref 0

  let cleanup _ = match !status with
    | None -> raise Cleanup_without_init
    | Some Cleaned -> raise Already_cleaned
    | Some Inited -> status := Some Cleaned

  let run c r = match c with
    | Get   -> Res (int, !r)
    | Set i -> Res (unit, (r:=i))
    | Add i -> Res (unit, let old = !r in r := i + old) (* buggy: not atomic *)

  let precond _ _ = true

  let postcond c (s:state) res = match c,res with
    | Get,   Res ((Int,_),r) -> if r>70 then raise Random_postcond_failure; r = s
    | Set _, Res ((Unit,_),_)
    | Add _, Res ((Unit,_),_) -> true
    | _,_ -> false
end

module RT_seq = STM_sequential.Make(RConf)
module RT_dom = STM_domain.Make(RConf)

let rand = Random.State.make_self_init ()
let i = ref 0
;;
for _i=1 to 250 do
  try
    Test.check_exn ~rand (RT_seq.agree_test ~count:1000 ~name:"STM ensure cleanup test sequential")
  with _e -> incr i; assert (!status = Some Cleaned);
done;
assert (!i = 250);
Printf.printf "STM ensure cleanup: sequential test OK\n%!";
(* reset things *)
i := 0;
status := None;
for _i=1 to 100 do
  try
    Test.check_exn ~rand
      (Test.make ~count:1000 ~name:"STM ensure cleanup test parallel"
         (RT_dom.arb_cmds_triple 20 12) RT_dom.agree_prop_par) (* without retries *)
  with _e -> incr i; assert (!status = Some Cleaned);
done;
assert (!i = 100);
Printf.printf "STM ensure cleanup: parallel test OK\n%!";
