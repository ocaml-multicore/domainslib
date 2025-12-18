(* ppx_heartbeat - A PPX for automatic heartbeat-based parallelism

   Usage:

   (* Simple fork-join *)
   let%par2 (a, b) = expr1, expr2 in
   a + b

   (* With pool *)
   let%par2 (a, b) = pool => expr1, expr2 in
   a + b

   Transforms to:

   let (a, b) =
     if Heartbeat.use_token () then begin
       (* Parallel: create closures, spawn, join *)
       let promise = Heartbeat.async pool (fun () -> expr1) in
       let b = expr2 in
       let a = Heartbeat.await pool promise in
       (a, b)
     end else begin
       (* Sequential: evaluate directly, no closures *)
       let a = expr1 in
       let b = expr2 in
       (a, b)
     end
   in
   a + b
*)

open Ppxlib

let expand_par2 ~ctxt pool_opt e1 e2 =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in

  (* Generate the parallel branch *)
  let parallel_branch = [%expr
    let promise = Heartbeat.async [%e pool_opt] (fun () -> [%e e1]) in
    let b = [%e e2] in
    let a = Heartbeat.await [%e pool_opt] promise in
    (a, b)
  ] in

  (* Generate the sequential branch - NO closures *)
  let sequential_branch = [%expr
    let a = [%e e1] in
    let b = [%e e2] in
    (a, b)
  ] in

  (* Combined with token check BEFORE any closure creation *)
  [%expr
    if Heartbeat.use_token () then
      [%e parallel_branch]
    else
      [%e sequential_branch]
  ]

(* Pattern: let%par2 (a, b) = e1, e2 *)
let par2_extension =
  Extension.V3.declare
    "par2"
    Extension.Context.expression
    Ast_pattern.(
      pstr (pstr_value nonrecursive (
        value_binding ~pat:(ppat_tuple (__ ^:: __ ^:: nil))
                      ~expr:(pexp_tuple (__ ^:: __ ^:: nil))
        ^:: nil) ^:: nil))
    (fun ~ctxt _pat1 _pat2 e1 e2 ->
      expand_par2 ~ctxt [%expr pool] e1 e2)

let () =
  Driver.register_transformation
    ~extensions:[par2_extension]
    "ppx_heartbeat"
