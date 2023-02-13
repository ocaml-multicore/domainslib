open STM

module Make (Spec: Spec) = struct

  open Util
  open QCheck
  open Internal.Make(Spec)
    [@alert "-internal"]

  exception ThreadNotFinished

  let arb_cmds_triple = arb_cmds_triple

  (* [interp_sut_res] specialized for [Threads] *)
  let rec interp_sut_res sut cs = match cs with
    | [] -> []
    | c::cs ->
       Thread.yield ();
       let res = Spec.run c sut in
       (c,res)::interp_sut_res sut cs

  (* Concurrent agreement property based on [Threads] *)
  let agree_prop_conc (seq_pref,cmds1,cmds2) =
    assume (all_interleavings_ok seq_pref cmds1 cmds2 Spec.init_state);
    let sut = Spec.init_sut () in
    let obs1,obs2 = ref (Error ThreadNotFinished), ref (Error ThreadNotFinished) in
    let pref_obs = interp_sut_res sut seq_pref in
    let wait = ref true in
    let th1 = Thread.create (fun () -> while !wait do Thread.yield () done; obs1 := try Ok (interp_sut_res sut cmds1) with exn -> Error exn) () in
    let th2 = Thread.create (fun () -> wait := false; obs2 := try Ok (interp_sut_res sut cmds2) with exn -> Error exn) () in
    let ()   = Thread.join th1 in
    let ()   = Thread.join th2 in
    let ()   = Spec.cleanup sut in
    let obs1 = match !obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match !obs2 with Ok v -> v | Error exn -> raise exn in
    check_obs pref_obs obs1 obs2 Spec.init_state
      || Test.fail_reportf "  Results incompatible with linearized model\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
           (fun (c,r) -> Printf.sprintf "%s : %s" (Spec.show_cmd c) (show_res r))
           (pref_obs,obs1,obs2)

  let agree_test_conc ~count ~name =
    (* a bigger [rep_count] for [Threads] as it is more difficult to trigger a problem *)
    let rep_count = 100 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make ~retries:15 ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (repeat rep_count agree_prop_conc) (* 100 times each, then 100 * 15 times when shrinking *)

  let neg_agree_test_conc ~count ~name =
    let rep_count = 25 in
    let seq_len,par_len = 20,12 in
    let max_gen = 3*count in (* precond filtering may require extra generation: max. 3*count though *)
    Test.make_neg ~retries:15 ~max_gen ~count ~name
      (arb_cmds_triple seq_len par_len)
      (repeat rep_count agree_prop_conc) (* 25 times each, then 25 * 15 times when shrinking *)
  end
