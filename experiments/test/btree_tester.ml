let print_btree tree = print_endline (Btree.show (fun fmt vl -> Format.fprintf fmt "\"%s\"" vl) tree)

let dump_btree fname (btree: string Btree.t) =
  Out_channel.with_open_bin fname (fun oc -> Marshal.to_channel oc btree [])

let read_btree fname : string Btree.t =
  In_channel.with_open_bin fname (fun ic -> (Marshal.from_channel ic : string Btree.t))

module IBB = Impbatch_btree.ImpBatchedBtree
let dump_impbtree fname (ibtree: IBB.tt) =
  Out_channel.with_open_bin fname (fun oc -> Marshal.to_channel oc ibtree [])

let print_impbtree (ibtree : IBB.tt) = 
  let btree = IBB.get_ds ibtree in
  IBB.print_tree btree

let read_impbtree fname : IBB.tt =
  In_channel.with_open_bin fname (fun ic -> (Marshal.from_channel ic : IBB.tt))

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
  | "par-search" :: fname :: keys :: (([] | [_])  as rest) ->
    let keys = String.split_on_char ',' keys |> List.map int_of_string |> Array.of_list in
    let tree = read_btree fname in
    let num_domains = match rest with
      | [] -> Domain.recommended_domain_count () 
      | domains_count :: _ -> int_of_string domains_count
    [@@alert "-unstable"] in
    let pool = Domainslib.Task.setup_pool ~num_domains:num_domains () in
    let results = Btree.par_search ~pool tree keys in
    for i = 0 to Array.length results - 1 do
      begin match results.(i) with
        | None -> print_endline (string_of_int keys.(i) ^ " ==> None")
        | Some res -> print_endline (string_of_int keys.(i) ^ " ==> " ^ res)
      end
    done

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
  | ["init_impbatch"; fname] -> 
    let num_domains = Domain.recommended_domain_count () - 1
    [@@alert "-unstable"] in
    let pool = Domainslib.Task.setup_pool ~num_domains:num_domains () in
    let btree = IBB.create pool in
    dump_impbtree fname btree;
    Domainslib.Task.teardown_pool pool
  | ["print_impbtree"; fname] ->
    let impbtree = read_impbtree fname in
    print_impbtree impbtree;
  | _ -> failwith "invalid arguments"
