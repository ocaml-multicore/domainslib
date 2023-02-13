open QCheck
open STM

(** parallel STM tests of Array *)

module AConf =
struct
  type cmd =
    | Length
    | Get of int
    | Set of int * char
    | Sub of int * int
    | Copy
    | Fill of int * int * char
    | To_list
    | Mem of char
    | Sort
    | To_seq
  [@@deriving show { with_path = false }]

  type state = char list
  type sut = char Array.t

  let arb_cmd s =
    let int_gen = Gen.(oneof [small_nat; int_bound (List.length s - 1)]) in
    let char_gen = Gen.printable in
    QCheck.make ~print:show_cmd (*~shrink:shrink_cmd*)
      Gen.(oneof
             [ return Length;
               map (fun i -> Get i) int_gen;
               map2 (fun i c -> Set (i,c)) int_gen char_gen;
               map2 (fun i len -> Sub (i,len)) int_gen int_gen; (* hack: reusing int_gen for length *)
               return Copy;
               map3 (fun i len c -> Fill (i,len,c)) int_gen int_gen char_gen; (* hack: reusing int_gen for length *)
               return To_list;
               map (fun c -> Mem c) char_gen;
               return Sort;
               return To_seq;
             ])

  let array_size = 16

  let init_state  = List.init array_size (fun _ -> 'a')

  let next_state c s = match c with
    | Length -> s
    | Get _  -> s
    | Set (i,c) ->
      List.mapi (fun j c' -> if i=j then c else c') s
    | Sub (_,_) -> s
    | Copy -> s
    | Fill (i,l,c) ->
      if i >= 0 && l >= 0 && i+l-1 < List.length s
      then
        List.mapi (fun j c' -> if i <= j && j <= i+l-1 then c else c') s
      else s
    | To_list -> s
    | Mem _ -> s
    | Sort -> List.sort Char.compare s
    | To_seq -> s

  let init_sut () = Array.make array_size 'a'
  let cleanup _   = ()

  let precond c _s = match c with
    | _ -> true

  let run c a = match c with
    | Length       -> Res (int, Array.length a)
    | Get i        -> Res (result char exn, protect (Array.get a) i)
    | Set (i,c)    -> Res (result unit exn, protect (Array.set a i) c)
    | Sub (i,l)    -> Res (result (array char) exn, protect (Array.sub a i) l)
    | Copy         -> Res (array char, Array.copy a)
    | Fill (i,l,c) -> Res (result unit exn, protect (Array.fill a i l) c)
    | To_list      -> Res (list char, Array.to_list a)
    | Mem c        -> Res (bool, Array.mem c a)
    | Sort         -> Res (unit, Array.sort Char.compare a)
    | To_seq       -> Res (seq char, List.to_seq (List.of_seq (Array.to_seq a))) (* workaround: Array.to_seq is lazy and will otherwise see and report later Array.set state changes... *)

  let postcond c (s:char list) res = match c, res with
    | Length, Res ((Int,_),i) -> i = List.length s
    | Get i, Res ((Result (Char,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok (List.nth s i)
    | Set (i,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || i >= List.length s
      then r = Error (Invalid_argument "index out of bounds")
      else r = Ok ()
    | Sub (i,l), Res ((Result (Array Char,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Array.sub")
      else r = Ok (Array.of_list (List.filteri (fun j _ -> i <= j && j <= i+l-1) s))
    | Copy, Res ((Array Char,_),r) -> Array.to_list r = s
    | Fill (i,l,_), Res ((Result (Unit,Exn),_), r) ->
      if i < 0 || l < 0 || i+l > List.length s
      then r = Error (Invalid_argument "Array.fill")
      else r = Ok ()
    | To_list, Res ((List Char,_),cs) -> cs = s
    | Mem c, Res ((Bool,_),r) -> r = List.mem c s
    | Sort, Res ((Unit,_),r) -> r = ()
    | To_seq, Res ((Seq Char,_),r) -> Seq.equal (=) r (List.to_seq s)
    | _, _ -> false
end

module ArraySTM_seq = STM_sequential.Make(AConf)
module ArraySTM_dom = STM_domain.Make(AConf)
;;
QCheck_base_runner.run_tests_main
  (let count = 1000 in
   [ArraySTM_seq.agree_test         ~count ~name:"STM Array test sequential";
    ArraySTM_dom.neg_agree_test_par ~count ~name:"STM Array test parallel" (* this test is expected to fail *)
])
