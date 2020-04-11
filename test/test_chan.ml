let buffer_size = try int_of_string Sys.argv.(1) with _ -> 1
let num_items = try int_of_string Sys.argv.(2) with _ -> 100
let num_senders = try int_of_string Sys.argv.(3) with _ -> 1
let num_receivers = try int_of_string Sys.argv.(4) with _ -> 1

module C = Domainslib.Chan

let c = C.make_bounded buffer_size

let rec receiver i n =
  if i = n then
    print_endline @@ Printf.sprintf "Receiver on domain %d done" (Domain.self () :> int)
  else (
    ignore @@ C.recv c;
    receiver (i+1) n )

let rec sender i n =
  if i = n then
    print_endline @@ Printf.sprintf "Sender on domain %d done" (Domain.self () :> int)
  else (
    C.send c i;
    sender (i+1) n )

let _ =
  assert (num_items mod num_senders == 0);
  assert (num_items mod num_receivers == 0);
  let senders =
    Array.init num_senders (fun _ ->
      Domain.spawn (fun _ -> sender 0 (num_items / num_senders)))
  in
  let receivers =
    Array.init num_receivers (fun _ ->
      Domain.spawn (fun _ -> receiver 0 (num_items / num_receivers)))
  in
  Array.iter Domain.join senders;
  Array.iter Domain.join receivers;
  begin match C.recv_poll c with
  | None -> ()
  | Some _ -> assert false
  end;
  for _i=1 to buffer_size do
    C.send c 0
  done;
  for _i=1 to buffer_size do
    ignore (C.recv c)
  done;
  begin match C.recv_poll c with
  | None -> ()
  | Some _ -> assert false
  end
