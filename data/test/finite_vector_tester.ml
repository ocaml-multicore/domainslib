open Data

let write file (v: int Finite_vector.t)  : unit =
  Out_channel.with_open_bin file (fun oc -> Marshal.to_channel oc v []) 
let read file : int Finite_vector.t = In_channel.with_open_bin file Marshal.from_channel 
let print v = Format.printf "%a\n%!" (Finite_vector.pp Format.pp_print_int) v

let with_vector file f =
  let vec = read file in
  f vec;
  write file vec

let () =
  match List.init (Array.length Sys.argv - 1) (fun i -> Sys.argv.(i + 1)) with
  | "init" :: file :: capacity :: _ -> write file (Finite_vector.init ~capacity:(int_of_string capacity) ())
  | "init" :: file :: _ -> write file (Finite_vector.init ())
  | "singleton" :: file :: vl :: capacity :: _ ->
    write file (Finite_vector.singleton ~capacity:(int_of_string capacity) (int_of_string vl))
  | "singleton" :: file :: vl :: _ -> write file (Finite_vector.singleton (int_of_string vl))
  | "print" :: file :: _ -> print (read file)
  | "get" :: file :: index :: _ -> 
    let index = int_of_string index in
    Format.printf "%d\n%!" (Finite_vector.get (read file) index)
  | "insert" :: file :: i :: vl :: _ ->
    let i = int_of_string i and vl = int_of_string vl in
    with_vector file (fun vec -> Finite_vector.insert vec i vl)
  | "drop_last" :: file :: _ ->
    with_vector file (fun vec -> Finite_vector.drop_last vec)
  | "split_from" :: ifile :: ofile :: index :: _ ->
    let index = int_of_string index in
    with_vector ifile (fun vec -> write ofile (Finite_vector.split_from vec index))
  | "clip" :: file :: index :: _ ->
    let index = int_of_string index in
    with_vector file (fun vec -> Finite_vector.clip vec index)
  | _ -> failwith "invalid arguments"
