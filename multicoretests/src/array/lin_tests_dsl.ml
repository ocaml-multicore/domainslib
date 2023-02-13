(* ********************************************************************** *)
(*                      Tests of thread-unsafe [Array]                    *)
(* ********************************************************************** *)
module AConf =
struct
  type t = char array

  let array_size = 16
  let init () = Array.make array_size 'a'
  let cleanup _ = ()

  open Lin
  let int,char = nat_small,char_printable
  let array_to_seq a = List.to_seq (List.of_seq (Array.to_seq a)) (* workaround: Array.to_seq is lazy and will otherwise see and report later Array.set state changes... *)
  let api =
    [ val_ "Array.length"   Array.length   (t @-> returning int);
      val_ "Array.get"      Array.get      (t @-> int @-> returning_or_exc char);
      val_ "Array.set"      Array.set      (t @-> int @-> char @-> returning_or_exc unit);
      val_ "Array.sub"      Array.sub      (t @-> int @-> int @-> returning_or_exc (array char));
      val_ "Array.copy"     Array.copy     (t @-> returning (array char));
      val_ "Array.fill"     Array.fill     (t @-> int @-> int @-> char @-> returning_or_exc unit);
      val_ "Array.to_list"  Array.to_list  (t @-> returning (list char));
      val_ "Array.mem"      Array.mem      (char @-> t @-> returning bool);
      val_ "Array.sort"     (Array.sort Char.compare) (t @-> returning unit);
      val_ "Array.to_seq"   array_to_seq   (t @-> returning (seq char));
    ]
end

module AT_domain = Lin_domain.Make(AConf)
;;
QCheck_base_runner.run_tests_main [
  AT_domain.neg_lin_test ~count:1000 ~name:"Lin DSL Array test with Domain";
]
