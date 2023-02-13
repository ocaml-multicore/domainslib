open STM

module Make (Spec: Spec) = struct

  open QCheck
  open Internal.Make(Spec)
    [@alert "-internal"]

  (* re-export some functions *)
  let cmds_ok        = cmds_ok
  let arb_cmds       = arb_cmds

  let print_seq_trace trace =
    List.fold_left
      (fun acc (c,r) -> Printf.sprintf "%s\n   %s : %s" acc (Spec.show_cmd c) (show_res r))
      "" trace

  let agree_prop cs =
    assume (cmds_ok Spec.init_state cs);
    let sut = Spec.init_sut () in (* reset system's state *)
    let res = try Ok (check_disagree Spec.init_state sut cs) with exn -> Error exn in
    let ()  = Spec.cleanup sut in
    let res = match res with Ok res -> res | Error exn -> raise exn in
    match res with
    | None -> true
    | Some trace ->
      Test.fail_reportf "  Results incompatible with model\n%s"
      @@ print_seq_trace trace

  let agree_test ~count ~name =
    Test.make ~name ~count (arb_cmds Spec.init_state) agree_prop

  let neg_agree_test ~count ~name =
    Test.make_neg ~name ~count (arb_cmds Spec.init_state) agree_prop

  end
