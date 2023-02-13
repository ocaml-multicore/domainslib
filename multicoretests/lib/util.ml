let rec repeat n prop = fun input ->
  if n<0 then failwith "repeat: negative repetition count";
  if n=0
  then true
  else prop input && repeat (n-1) prop input

exception Timeout

let prop_timeout sec p x =
  Sys.(signal sigalrm (Signal_handle (fun _ -> raise Timeout))) |> ignore;
  ignore (Unix.alarm sec);
  let res = p x in
  ignore (Unix.alarm 0); (*cancel alarm*)
  res

let fork_prop_with_timeout sec p x =
  let a = Unix.fork () in
  match a with
  | 0  ->
    let _ = Unix.alarm sec in
    if p x
    then (ignore (Unix.alarm 0); exit 0) (*cancel alarm*)
    else (ignore (Unix.alarm 0); exit 2) (*cancel alarm*)
  | _  ->
    let _childid, retcode = Unix.wait () in
    (match retcode with
     | WEXITED code -> (0=code)
     | WSIGNALED s when s = Sys.sigalrm -> raise Timeout
     | WSIGNALED _
     | WSTOPPED _  -> false)

let print_vertical ?(fig_indent=3) show cmds =
  let cmds = List.map show cmds in
  let buf = Buffer.create 64 in
  let indent () = Printf.bprintf buf "%s" (String.make fig_indent ' ') in
  let print_seq_col c = Printf.bprintf buf "%s\n" c in
  let () = List.iter (fun c -> indent (); print_seq_col c) cmds in
  Buffer.contents buf

let print_triple_vertical ?(fig_indent=10) ?(res_width=20) ?(center_prefix=true) show (seq,cmds1,cmds2) =
  let seq,cmds1,cmds2 = List.(map show seq, map show cmds1, map show cmds2) in
  let max_width ss = List.fold_left max 0 (List.map String.length ss) in
  let width = List.fold_left max 0 [max_width seq; max_width cmds1; max_width cmds2] in
  let res_width = max width res_width in
  let cmd_indent = String.make ((width-1)/2) ' ' in
  let seq_indent = String.make ((res_width + 3)/2) ' ' in
  let bar_cmd = Printf.sprintf "%-*s" res_width (cmd_indent ^ "|") in
  let center c =
    let clen = String.length c in
    if clen > width (* it's a '|'-string *)
    then c
    else Printf.sprintf "%s%s" (String.make ((width - clen)/2) ' ') c in
  let buf = Buffer.create 64 in
  let indent () = Printf.bprintf buf "%s" (String.make fig_indent ' ') in
  let print_seq_col c = Printf.bprintf buf "%s%-*s\n" seq_indent res_width c in
  let print_par_col c1 c2 = Printf.bprintf buf "%-*s  %-*s\n" res_width c1 res_width c2 in
  let print_hoz_line () =
    Printf.bprintf buf "%-*s\n" res_width (cmd_indent ^ "." ^ (String.make (res_width + 1) '-') ^ ".") in
  let rec print_par_cols cs cs' = match cs,cs' with
    | [],   []    -> ()
    | c::cs,[]    -> indent (); print_par_col (center c) ""; print_par_cols cs []
    | [],   c::cs -> indent (); print_par_col "" (center c); print_par_cols [] cs
    | l::ls,r::rs -> indent (); print_par_col (center l) (center r); print_par_cols ls rs in
  (* actual printing *)
  if center_prefix
  then
    List.iter (fun c -> indent (); print_seq_col (center c)) ([bar_cmd] @ seq @ [bar_cmd])
  else
    List.iter (fun c -> indent (); print_par_col (center c) "") (bar_cmd::seq@[bar_cmd]);
  indent (); print_hoz_line ();
  print_par_cols (bar_cmd::cmds1) (bar_cmd::cmds2);
  Buffer.contents buf

let protect (f : 'a -> 'b) (a : 'a) : ('b, exn) result =
  try Result.Ok (f a)
  with e -> Result.Error e

let pp_exn fmt e = Format.fprintf fmt "%s" (Printexc.to_string e)
let show_exn e = Format.asprintf "%a" e pp_exn
let equal_exn = (=)
