let print_btree tree = print_endline (Btree.show (fun fmt vl -> Format.fprintf fmt "\"%s\"" vl) tree)

let dump_btree fname (btree: string Btree.t) =
  Out_channel.with_open_bin fname (fun oc -> Marshal.to_channel oc btree [])

let read_btree fname : string Btree.t =
  In_channel.with_open_bin fname (fun ic -> (Marshal.from_channel ic : string Btree.t))

let () =
  let args = List.init (Array.length Sys.argv - 1) (fun i -> Sys.argv.(i + 1)) in
  match args with
  | ["init"; fname] ->
    let btree: string Btree.t = Btree.create () in
    dump_btree fname btree
  | ["init"; fname; max_size] ->
    let max_keys = int_of_string max_size in
    let btree: string Btree.t = Btree.create ~max_keys () in
    dump_btree fname btree
  | ["print"; fname] ->
    let tree = read_btree fname in
    print_btree tree
  | ["search"; fname; key] ->
    let key = int_of_string key in
    let tree = read_btree fname in
    begin match Btree.search tree key with
    | None -> print_endline "None"
    | Some k -> print_endline ("\"" ^ k ^ "\"")
    end
  | ["add"; fname; k; vl] ->
    let k = int_of_string k in
    let btree = read_btree fname in
    Btree.insert btree k vl;
    dump_btree fname btree
  | ["add-and-print"; fname; k; vl] ->
    let k = int_of_string k in
    let btree = read_btree fname in
    Btree.insert btree k vl;
    dump_btree fname btree;
    print_btree btree
  | _ -> failwith "invalid arguments"
