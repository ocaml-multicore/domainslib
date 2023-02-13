type cmd = A | B | C

let show_cmd c = match c with
  | A -> "A"
  | B -> "B"
  | C -> "C"

let print_and_check_output s res =
  let ss = (String.split_on_char '\n' s) in
  List.iter (fun s -> Printf.printf "\"%s\"\n%!" s) ss;
  assert (List.equal String.equal res ss)

;;
let s1 = Util.print_triple_vertical ~fig_indent:1 show_cmd ([A],[A;B;C],[C]) in
let res1 = [
  "            |                   ";
  "            A                   ";
  "            |                   ";
  " .---------------------.";
  " |                     |                   ";
  " A                     C                   ";
  " B                                         ";
  " C                                         ";
  ""]
in
print_and_check_output s1 res1
(*assert (List.equal String.equal res1 ss1)*)
;;
let s2 = Util.print_triple_vertical ~fig_indent:1 ~center_prefix:false show_cmd ([A],[A;B;C],[C]) in
let res2 = [
  " |                                         ";
  " A                                         ";
  " |                                         ";
  " .---------------------.";
  " |                     |                   ";
  " A                     C                   ";
  " B                                         ";
  " C                                         ";
  ""]
in
print_and_check_output s2 res2
