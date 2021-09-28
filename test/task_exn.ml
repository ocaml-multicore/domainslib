module T = Domainslib.Task

exception E

let _ =
  let p1 = T.async  (fun () ->
    let p2 = T.async (fun () -> raise E) in
    T.await p2)
  in
  begin match T.await p1 with
  | _ -> ()
  | exception E -> print_endline "Caught E"
  end;

  let _p1 = T.async (fun () ->
    let p2 = T.async (fun () ->
      let rec loop n =
        if n = 0 then ()
        else loop (n-1)
      in loop 100000000)
    in
    T.await p2)
  in
  match T.Pool.teardown_default_pool () with
  | _ -> ()
  | exception T.TasksActive ->
      (* innermost task may still be active *)
      print_endline "Caught TasksActive"
