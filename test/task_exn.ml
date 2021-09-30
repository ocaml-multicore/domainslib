module T = Domainslib.Task

exception E

let _ =
  let pool = T.setup_pool ~num_additional_domains:3 () in

  let p1 = T.async pool (fun () ->
    let p2 = T.async pool (fun () -> raise E) in
    T.await pool p2)
  in
  begin match T.await pool p1 with
  | _ -> ()
  | exception E -> print_endline "Caught E"
  end;

  let _p1 = T.async pool (fun () ->
    let p2 = T.async pool (fun () ->
      let rec loop n =
        if n = 0 then ()
        else loop (n-1)
      in loop 100000000)
    in
    T.await pool p2)
  in
  match T.teardown_pool pool with
  | _ -> ()
  | exception T.TasksActive ->
      (* innermost task may still be active *)
      print_endline "Caught TasksActive"
