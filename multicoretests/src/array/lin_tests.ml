open QCheck

(** ********************************************************************** *)
(**                      Tests of thread-unsafe [Array]                    *)
(** ********************************************************************** *)
module AConf =
struct
  type t = char array

  type cmd =
    | Length
    | Get of int'
    | Set of int' * char'
    | Sub of int' * int'
    | Copy
    | Fill of int' * int' * char'
    | To_list
    | Mem of char'
    | Sort
    | To_seq [@@deriving qcheck, show { with_path = false }]
  and int' = int [@gen Gen.small_nat]
  and char' = char [@gen Gen.printable]

  let shrink_cmd c = Iter.empty

  open Util
  (*let pp_exn = Util.pp_exn*)
  type res =
    | RLength of int
    | RGet of ((char, exn) result)
    | RSet of ((unit, exn) result)
    | RSub of ((char array, exn) result)
    | RCopy of char array
    | RFill of ((unit, exn) result)
    | RTo_list of char list
    | RMem of bool
    | RSort of unit
    | RTo_seq of (char Seq.t [@printer fun fmt seq -> fprintf fmt "%s" (QCheck.Print.(list char) (List.of_seq seq))])
  [@@deriving show { with_path = false }, eq]

  let array_size = 16

  let init () = Array.make array_size 'a'

  let run c a = match c with
    | Length        -> RLength (Array.length a)
    | Get i         -> RGet (Util.protect (Array.get a) i)
    | Set (i,c)     -> RSet (Util.protect (Array.set a i) c)
    | Sub (i,l)     -> RSub (Util.protect (Array.sub a i) l)
    | Copy          -> RCopy (Array.copy a)
    | Fill (i,l,c)  -> RFill (Util.protect (Array.fill a i l) c)
    | To_list       -> RTo_list (Array.to_list a)
    | Mem c         -> RMem (Array.mem c a)
    | Sort          -> RSort (Array.sort Char.compare a)
    | To_seq        -> RTo_seq (List.to_seq (List.of_seq (Array.to_seq a))) (* workaround: Array.to_seq is lazy and will otherwise see and report later Array.set state changes... *)
  let cleanup _ = ()
end

module AT_domain = Lin_domain.Make_internal(AConf) [@alert "-internal"]
;;
QCheck_base_runner.run_tests_main [
  AT_domain.neg_lin_test ~count:1000 ~name:"Lin Array test with Domain";
]
