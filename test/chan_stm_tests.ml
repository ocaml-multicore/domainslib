open QCheck
open Domainslib
open STM

(** This contains sequential and parallel model-based tests of [Domainslib.Chan] *)

module ChConf =
struct
  type state = int list
  type sut = int Domainslib.Chan.t
  type cmd =
    | Send of int
    | Send_poll of int
    | Recv
    | Recv_poll

  let show_cmd c = match c with
    | Send i -> "Send" ^ (string_of_int i)
    | Send_poll i -> "Send_poll" ^ (string_of_int i)
    | Recv -> "Recv"
    | Recv_poll -> "Recv_poll"

  let capacity = 8

  let arb_cmd s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (if s=[]
       then
         Gen.oneof
           [Gen.map (fun i -> Send i) int_gen;
	    Gen.map (fun i -> Send_poll i) int_gen;
	    Gen.return Recv_poll] (* don't generate blocking Recv cmds on an empty channel *)
       else
       if List.length s >= capacity
       then
         Gen.oneof
           [Gen.map (fun i -> Send_poll i) int_gen;
            Gen.return Recv;
	    Gen.return Recv_poll] (* don't generate blocking Send cmds on a full channel *)
       else
         Gen.oneof
           [Gen.map (fun i -> Send i) int_gen;
	    Gen.map (fun i -> Send_poll i) int_gen;
            Gen.return Recv;
	    Gen.return Recv_poll])
  let init_state  = []
  let init_sut () = Chan.make_bounded capacity
  let cleanup _   = ()

  let next_state c s = match c with
    | Send i      -> if List.length s < capacity then s@[i] else s
    | Send_poll i -> if List.length s < capacity then s@[i] else s
    | Recv        -> begin match s with [] -> [] | _::s' -> s' end
    | Recv_poll   -> begin match s with [] -> [] | _::s' -> s' end

  let precond c s = match c,s with
    | Recv,   [] -> false
    | Send _, _  -> List.length s < capacity
    | _,      _  -> true

  let run c chan =
    match c with
    | Send i       -> Res (unit, Chan.send chan i)
    | Send_poll i  -> Res (bool, Chan.send_poll chan i)
    | Recv         -> Res (int, Chan.recv chan)
    | Recv_poll    -> Res (option int, Chan.recv_poll chan)

  let postcond c s res = match c,res with
    | Send _,      Res ((Unit,_),_) -> (List.length s < capacity)
    | Send_poll _, Res ((Bool,_),res) -> res = (List.length s < capacity)
    | Recv,        Res ((Int,_),res) -> (match s with [] -> false | res'::_ -> Int.equal res res')
    | Recv_poll,   Res ((Option Int,_),opt) -> (match s with [] -> None | res'::_ -> Some res') = opt
    | _,_ -> false
end


module ChT_seq = STM_sequential.Make(ChConf)
module ChT_dom = STM_domain.Make(ChConf)

let () =
  let count = 500 in
  QCheck_base_runner.run_tests_main [
    ChT_seq.agree_test     ~count ~name:"STM Domainslib.Chan test sequential";
    ChT_dom.agree_test_par ~count ~name:"STM Domainslib.Chan test parallel";
  ]
