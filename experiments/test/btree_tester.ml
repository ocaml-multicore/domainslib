let print_btree tree = print_endline (Btree.show (fun fmt vl -> Format.fprintf fmt "\"%s\"" vl) tree)

let dump_btree fname (btree: string Btree.t) =
  Out_channel.with_open_bin fname (fun oc -> Marshal.to_channel oc btree [])

let read_btree fname : string Btree.t =
  In_channel.with_open_bin fname (fun ic -> (Marshal.from_channel ic : string Btree.t))

module StrIBB = Ib_btree

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
    let results = Batch_para_btree.par_search ~pool tree keys in
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
  | "par-insert" :: fname :: keys :: (([] | [_])  as rest) ->
    let keys = String.split_on_char ',' keys |> List.map int_of_string |> Array.of_list in
    let keys_vals = Array.map (fun k -> (k, "key " ^ string_of_int k)) keys in
    let tree = read_btree fname in
    let num_domains = match rest with
      | [] -> Domain.recommended_domain_count () 
      | domains_count :: _ -> int_of_string domains_count
    [@@alert "-unstable"] in
    let pool = Domainslib.Task.setup_pool ~num_domains:num_domains () in
    let[@warning "-21"] () = Batch_para_btree.par_insert ~pool tree keys_vals in
    dump_btree fname tree
  | ["init_impbatch"; fname] -> 
    let num_domains = Domain.recommended_domain_count () - 1
    [@@alert "-unstable"] in
    let pool = Domainslib.Task.setup_pool ~num_domains:num_domains () in
    let ibtree = StrIBB.create pool in
    let btree = StrIBB.unload ibtree in
    print_btree btree;
    dump_btree fname btree;
    Domainslib.Task.teardown_pool pool
  | ["imp_insert"; fname; n] ->
    let n = int_of_string n in
    let num_domains = Domain.recommended_domain_count () - 1
    [@@alert "-unstable"] in
    let pool = Domainslib.Task.setup_pool ~num_domains:num_domains () in
    let btree = read_btree fname in
    let ibtree =  StrIBB.create pool in
    StrIBB.load ibtree btree;
    for _ = 1 to n do
      let key = Random.int 20 in
      let value = "key " ^ (string_of_int key) in
      StrIBB.insert ibtree key value
    done;
    print_btree btree;
    dump_btree fname btree;
    Domainslib.Task.teardown_pool pool
  | "build":: fname :: max_keys :: keys :: (([] | [_])  as rest) ->
    let max_keys = int_of_string max_keys in
    let keys = String.split_on_char ',' keys |> List.map int_of_string |> Array.of_list in
    let keys_vals = Array.map (fun k -> (k, "key " ^ string_of_int k)) keys in
    let num_domains = match rest with
      | [] -> Domain.recommended_domain_count () 
      | domains_count :: _ -> int_of_string domains_count
    [@@alert "-unstable"] in
    let pool = Domainslib.Task.setup_pool ~num_domains:num_domains () in
    let btree = 
      Domainslib.Task.run pool (fun () -> Batch_para_btree.build ~max_keys pool keys_vals) in
    dump_btree fname btree;
    print_btree btree;
    Domainslib.Task.teardown_pool pool
  | _ -> failwith "invalid arguments"
