let rec iter_insert x ls f =
  match ls with
  | [] -> f [x]
  | h :: t ->
    f (x :: h :: t);
    iter_insert x t (fun ls -> f (h :: ls))

let rec iter_permutations ls f =
  match ls with
  | [] -> f []
  | hd :: tl ->
    iter_permutations tl (fun tl ->
        iter_insert hd tl f
      )

open struct
  module Internal =
  struct
    open QCheck
    include Util

    module type CmdSpec = sig
      type t
      (** The type of the system under test *)

      type cmd
      (** The type of commands *)

      val show_cmd : cmd -> string
      (** [show_cmd c] returns a string representing the command [c]. *)

      val gen_cmd : cmd Gen.t
      (** A command generator. *)

      val shrink_cmd : cmd Shrink.t
      (** A command shrinker.
          To a first approximation you can use [Shrink.nil]. *)

      type res
      (** The command result type *)

      val show_res : res -> string
      (** [show_res r] returns a string representing the result [r]. *)

      val equal_res : res -> res -> bool

      val init : unit -> t
      (** Initialize the system under test. *)

      val cleanup : t -> unit
      (** Utility function to clean up [t] after each test instance,
          e.g., for closing sockets, files, or resetting global parameters *)

      val run: cmd -> t -> res
      (** [run c t] should interpret command [c] over the system under test [t]. *)

      val run_batched : Domainslib.Task.pool -> t -> cmd list -> (cmd * res) list
        (** [run_batched cs t] should interpret the command [cs] over the system under test [t]. *)

    end

    (** A functor to create test setups, for all backends (Domain, Thread and Effect).
        We use it below, but it can also be used independently *)
    module Make(Spec : CmdSpec) = struct

      (* plain interpreter of a cmd list *)
      (* let interp_plain sut cs = List.map (fun c -> (c, Spec.run c sut)) cs *)

      let rec gen_cmds fuel =
        Gen.(if fuel = 0
             then return []
             else
               Spec.gen_cmd >>= fun c ->
               gen_cmds (fuel-1) >>= fun cs ->
               return (c::cs))
      (** A fueled command list generator. *)

      let gen_cmds_size size_gen = Gen.sized_size size_gen gen_cmds

      let shrink_tuple (seq,p1) =
        let open Iter in
        (* Shrinking heuristic:
           First reduce the cmd list sizes as much as possible, since the interleaving
           is most costly over long cmd lists. *)
        (map (fun seq' -> (seq',p1)) (Shrink.list_spine seq))
        <+>
        (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s))
        <+>
        (map (fun p1' -> (seq,p1')) (Shrink.list_spine p1))
        <+>
        (* Secondly reduce the cmd data of individual list elements *)
        (map (fun seq' -> (seq',p1)) (Shrink.list_elems Spec.shrink_cmd seq))
        <+>
        (map (fun p1' -> (seq,p1')) (Shrink.list_elems Spec.shrink_cmd p1))


      let arb_cmds_tuple seq_len par_len =
        let gen_tuple =
          Gen.(int_range 2 (2*par_len) >>= fun dbl_plen ->
               let seq_pref_gen = gen_cmds_size (int_bound seq_len) in
               let par_gen1 = gen_cmds_size (return dbl_plen) in
               tup2 seq_pref_gen par_gen1) in
        make ~print:(fun (pre,par) -> (print_triple_vertical Spec.show_cmd (pre,par,par))) ~shrink:shrink_tuple gen_tuple

      (** [check_batched_cons pref cs1 cs2 seq_sut seq_trace] returns a
          boolean indicating whether there exists any permutation of
          [cs1] when applied to a data structure with operations
          [List.rev seq_trace @ prefix] applied to it will produce the outputs seen in [cs2] *)

      let rec check_permutation_explains state ops obs_arr =
        match ops with
        | [] -> true
        | (i, (op, _)) :: ops ->
          let (_, ob) = obs_arr.(i) in
          let res = Spec.run op state in
          if Spec.equal_res res ob
          then check_permutation_explains state ops obs_arr
          else false

      let check_for_sequential_explanation_inner pref cs1 cs1_arr =
        let exception Found in
        try
          iter_permutations cs1 (fun cs1' ->
            (* create a new state *)
            let state = Spec.init () in
            (* run the prefix *)
            List.iter (fun (op, _) -> ignore (Spec.run op state)) pref;
            (* check that from this state, cs1' explains cs1: *)
            let explains = check_permutation_explains state cs1' cs1_arr in

            Spec.cleanup state;
            if explains then
              raise Found
            );
          false
        with Found -> true

      let check_for_sequential_explanation pref cs1 =
        (* first check that our prefix is explained by the sequential execution *)
        let state = Spec.init () in
        let explains =
          let pref_arr = Array.of_list pref in
          let pref = List.mapi (fun i vl -> (i,vl)) pref in
          check_permutation_explains state pref pref_arr  in
        Spec.cleanup state;
        let cs1_arr = Array.of_list cs1 in
        let cs1 = List.mapi (fun i vl -> (i,vl)) cs1 in
        explains && check_for_sequential_explanation_inner pref cs1 cs1_arr

      (* Linearization test *)
      let lin_test ~rep_count ~retries ~count ~name ~lin_prop =
        let arb_cmd_tuple = arb_cmds_tuple 20 4 in
        Test.make ~count ~retries ~name
          arb_cmd_tuple (repeat rep_count lin_prop)

      (* Negative linearization test *)
      let neg_lin_test ~rep_count ~retries ~count ~name ~lin_prop =
        let arb_cmd_triple = arb_cmds_tuple 20 4 in
        Test.make_neg ~count ~retries ~name
          arb_cmd_triple (repeat rep_count lin_prop)
    end
  end
end

(* Type-representing values *)

type constructible = |
type deconstructible = |

type combinable
type noncombinable

type (_,_,_,_) ty =
  | Gen : 'a QCheck.arbitrary * ('a -> string) -> ('a, constructible, 's, combinable) ty
  | Deconstr : ('a -> string) * ('a -> 'a -> bool) -> ('a, deconstructible, 's, combinable) ty
  | GenDeconstr : 'a QCheck.arbitrary * ('a -> string) * ('a -> 'a -> bool) -> ('a, 'c, 's, combinable) ty
  | State : ('s, constructible, 's, noncombinable) ty

let gen gen print = Gen (gen,print)
let deconstructible print eq = Deconstr (print,eq)
let gen_deconstructible gen print eq = GenDeconstr (gen,print,eq)

let qcheck_nat64_small = QCheck.(map Int64.of_int small_nat)

let print_char c   = Printf.sprintf "%C" c
let print_string s = Printf.sprintf "%S" s

let unit =           GenDeconstr (QCheck.unit,           QCheck.Print.unit, (=))
let bool =           GenDeconstr (QCheck.bool,           QCheck.Print.bool, (=))
let char =           GenDeconstr (QCheck.char,           print_char,        (=))
let char_printable = GenDeconstr (QCheck.printable_char, print_char,        (=))
let nat_small =      GenDeconstr (QCheck.small_nat,      QCheck.Print.int,  (=))
let int =            GenDeconstr (QCheck.int,            QCheck.Print.int,  (=))
let int_small =      GenDeconstr (QCheck.small_int,      QCheck.Print.int,  (=))
let int_pos =        GenDeconstr (QCheck.pos_int,        QCheck.Print.int,  (=))
let int_bound b =    GenDeconstr (QCheck.int_bound b,    QCheck.Print.int,  (=))
let int32 =          GenDeconstr (QCheck.int32,          Int32.to_string,   Int32.equal)
let int64 =          GenDeconstr (QCheck.int64,          Int64.to_string,   Int64.equal)
let nat64_small =    GenDeconstr (qcheck_nat64_small,    Int64.to_string,   Int64.equal)
let float =          GenDeconstr (QCheck.float,          Float.to_string,   Float.equal)
let string =         GenDeconstr (QCheck.string,         print_string,      String.equal)
let string_small =   GenDeconstr (QCheck.small_string,   print_string,      String.equal)
let string_small_printable = GenDeconstr (QCheck.small_printable_string,   print_string,      String.equal)

let option : type a c s. ?ratio:float -> (a, c, s, combinable) ty -> (a option, c, s, combinable) ty =
  fun ?ratio ty ->
  match ty with
  | Gen (arb, print) -> Gen (QCheck.option ?ratio arb, QCheck.Print.option print)
  | GenDeconstr (arb, print, eq) -> GenDeconstr (QCheck.option ?ratio arb, QCheck.Print.option print, Option.equal eq)
  | Deconstr (print, eq) -> Deconstr (QCheck.Print.option print, Option.equal eq)
let opt = option

let list : type a c s. (a, c, s, combinable) ty -> (a list, c, s, combinable) ty =
  fun ty -> match ty with
    | Gen (arb, print) -> Gen (QCheck.list arb, QCheck.Print.list print)
    | GenDeconstr (arb, print, eq) -> GenDeconstr (QCheck.list arb, QCheck.Print.list print, List.equal eq)
    | Deconstr (print, eq) -> Deconstr (QCheck.Print.list print, List.equal eq)

let array : type a c s. (a, c, s, combinable) ty -> (a array, c, s, combinable) ty =
  fun ty -> match ty with
    | Gen (arb, print) -> Gen (QCheck.array arb, QCheck.Print.array print)
    | GenDeconstr (arb, print, eq) -> GenDeconstr (QCheck.array arb, QCheck.Print.array print, Array.for_all2 eq)
    | Deconstr (print, eq) -> Deconstr (QCheck.Print.array print, Array.for_all2 eq)

let print_seq pp s =
  let b = Buffer.create 25 in
  Buffer.add_char b '<';
  Seq.iteri (fun i x ->
      if i > 0 then Buffer.add_string b "; ";
      Buffer.add_string b (pp x))
    s;
  Buffer.add_char b '>';
  Buffer.contents b

let arb_seq a =
  let open QCheck in
  let print = match a.print with None -> None | Some ap -> Some (print_seq ap) in
  let shrink s = Iter.map List.to_seq (Shrink.list ?shrink:a.shrink (List.of_seq s)) in
  let gen = Gen.map List.to_seq (Gen.list a.gen) in
  QCheck.make ?print ~shrink gen

let seq : type a c s. (a, c, s, combinable) ty -> (a Seq.t, c, s, combinable) ty =
  fun ty -> match ty with
    | Gen (arb, print) -> Gen (arb_seq arb, print_seq print)
    | GenDeconstr (arb, print, eq) -> GenDeconstr (arb_seq arb, print_seq print, Seq.equal eq)
    | Deconstr (print, eq) -> Deconstr (print_seq print, Seq.equal eq)

let state = State
let t = state

let print_result print_ok print_err = function
  | Ok x    -> Printf.sprintf "Ok (%s)" (print_ok x)
  | Error y -> Printf.sprintf "Error (%s)" (print_err y)

let or_exn ty = match ty with
  | GenDeconstr (_, print, eq) ->
    Deconstr (print_result print Printexc.to_string, Result.equal ~ok:eq ~error:(=))
  | Deconstr (print, eq) ->
    Deconstr (print_result print Printexc.to_string, Result.equal ~ok:eq ~error:(=))

let print : type a c s comb. (a, c, s, comb) ty -> a -> string = fun ty value ->
  match ty with
  | Gen (_,print) -> print value
  | Deconstr (print,_) -> print value
  | GenDeconstr (_,print,_) -> print value
  | State -> "t"

let equal : type a s c. (a, deconstructible, s, c) ty -> a -> a -> bool = fun ty ->
  match ty with
  | Deconstr (_,equal) -> equal
  | GenDeconstr (_,_,equal) -> equal

module Fun = struct
  (* Function type, number of arguments (unary encoding), state type *)
  type (_,_,_,_,_) fn =
    | Ret :        ('a, deconstructible, 's, combinable) ty -> ('a, 'w, 'a, 's, 'w) fn
    | Ret_or_exc : ('a, deconstructible, 's, combinable) ty -> ('a, 'w, ('a,exn) result, 's, 'w) fn
    | Ret_ignore : ('a, _, 's, _) ty -> ('a, 'w, unit, 's, 'w) fn
    | Ret_ignore_or_exc : ('a, _, 's, _) ty -> ('a, 'w, (unit,exn) result, 's, 'w) fn
    | Fn : ('a, constructible, 's, _) ty * ('b, 'wf, 'r, 's, 'w) fn -> ('a -> 'b, 'a -> 'wf, 'r, 's, 'w) fn
end

let returning a = Fun.Ret a
let returning_or_exc a = Fun.Ret_or_exc a
let returning_ a = Fun.Ret_ignore a
let returning_or_exc_ a = Fun.Ret_ignore_or_exc a
let (@->) a b = Fun.Fn (a,b)

type (_,_) elem =
  | Elem :
      { name : string
      ; fntyp : ('ftyp, 'wftyp, 'r, 's ,'w) Fun.fn
      ; value : 'ftyp
      ; wrapped: ('r -> unit) -> 'wftyp
      }
      -> ('s, 'w) elem

type ('s, 'w) api = (int * ('s, 'w) elem) list

let val_ name value wrapped fntyp =
  (1, Elem { name ; fntyp ; value; wrapped })

let val_freq freq name value wrapped fntyp =
  (freq, Elem { name ; fntyp ; value; wrapped })

module type Spec = sig

  type t
  type wrapped_op

  val init : unit -> t
  val cleanup : t -> unit

  val run: Domainslib.Task.pool -> t -> wrapped_op list -> unit

  val api : (int * (t, wrapped_op) elem) list

end

open struct
  module MakeCmd (ApiSpec : Spec) : Internal.CmdSpec = struct

    type t = ApiSpec.t

    let init = ApiSpec.init
    let cleanup = ApiSpec.cleanup

    (* Typed argument list and return type descriptor *)
    module Args = struct
      type (_,_,_,_) args =
        | Ret : ('a, deconstructible, t, _) ty -> ('a,'w,'a,'w) args
        | Ret_or_exc : ('a, deconstructible, t, _) ty -> ('a,'w, ('a,exn) result,'w) args
        | Ret_ignore : ('a, _, t, _) ty -> ('a,'w, unit,'w) args
        | Ret_ignore_or_exc : ('a, _, t, _) ty -> ('a,'w, (unit,exn) result,'w) args
        | Fn : 'a * ('b,'wf,'r,'w) args -> ('a -> 'b, 'a -> 'wf, 'r,'w) args
        | FnState : ('b,'wf,'r,'w) args -> (t -> 'b, t -> 'wf, 'r,'w) args
    end

    (* Operation name, typed argument list, return type descriptor, printer, shrinker, function *)
    type cmd =
        Cmd :
          { name : string
          ; args : ('ftyp, 'wftyp, 'r, ApiSpec.wrapped_op) Args.args
          ; rty : ('r, deconstructible, t, _) ty
          ; print : (('ftyp, 'wftyp, 'r, ApiSpec.wrapped_op) Args.args -> string)
          ; shrink : (('ftyp, 'wftyp, 'r, ApiSpec.wrapped_op) Args.args QCheck.Shrink.t)
          ; f : 'ftyp
          ; wrapped_f: ('r -> unit) -> 'wftyp
          }
          -> cmd

    type res =
        Res : ('a, deconstructible, t, _) ty * 'a -> res

    (* Function to generate typed list of arguments from a function description.
       The printer can be generated independently. *)
    let rec gen_args_of_desc
      : type a wa r. (a, wa, r, t, ApiSpec.wrapped_op) Fun.fn ->
        ((a, wa, r, ApiSpec.wrapped_op) Args.args) QCheck.Gen.t =
      fun fdesc ->
      let open QCheck.Gen in
      match fdesc with
      | Fun.Ret ty -> return @@ Args.Ret ty
      | Fun.Ret_or_exc ty -> return @@ Args.Ret_or_exc ty
      | Fun.Ret_ignore_or_exc ty -> return @@ Args.Ret_ignore_or_exc ty
      | Fun.Ret_ignore ty -> return @@ Args.Ret_ignore ty
      | Fun.(Fn (State, fdesc_rem)) ->
        let* args_rem = gen_args_of_desc fdesc_rem in
        return @@ Args.FnState args_rem
      | Fun.(Fn ((Gen (arg_arb,_) | GenDeconstr (arg_arb, _, _)), fdesc_rem)) ->
        let* arg = arg_arb.gen in
        let* args_rem = gen_args_of_desc fdesc_rem in
        return @@ Args.Fn (arg, args_rem)

    let rec ret_type
      : type a wa r. (a,wa,r,t,ApiSpec.wrapped_op) Fun.fn ->
        (r, deconstructible, t, _) ty
      = fun fdesc ->
        match fdesc with
        | Fun.Ret ty -> ty
        | Fun.Ret_or_exc ty -> or_exn ty
        | Fun.Ret_ignore _ -> unit
        | Fun.Ret_ignore_or_exc _ -> or_exn unit
        | Fun.Fn (_, fdesc_rem) -> ret_type fdesc_rem

    let rec show_args : type a wa r.
      (a,wa,r,t,ApiSpec.wrapped_op) Fun.fn ->
      (a,wa,r,ApiSpec.wrapped_op) Args.args -> string list = fun fdesc args ->
      match fdesc,args with
      | _, Args.(Ret _ | Ret_or_exc _ | Ret_ignore _ | Ret_ignore_or_exc _) -> []
      | Fun.(Fn (State, fdesc_rem)), Args.(FnState args_rem) ->
        "t"::show_args fdesc_rem args_rem
      | Fun.(Fn ((GenDeconstr _ | Gen _ as ty), fdesc_rem)), Args.(Fn (value, args_rem)) ->
        (print ty value)::show_args fdesc_rem args_rem
      | Fun.(Fn (State, _)), Args.(Fn _)
      | Fun.(Fn ((Gen _ | GenDeconstr _), _)), Args.(FnState _)  ->
        assert false
      | Fun.(Ret _ | Ret_or_exc _ | Ret_ignore _ | Ret_ignore_or_exc _), Args.(Fn _ | FnState _) ->
        assert false

    let gen_printer :
      type a wa r. string -> (a,wa,r,t,ApiSpec.wrapped_op) Fun.fn ->
      (a,wa,r,ApiSpec.wrapped_op) Args.args -> string =
      fun name fdesc args ->
      name ^ " " ^ (String.concat " " (show_args fdesc args))

    (* Extracts a QCheck shrinker for argument lists *)
    let rec gen_shrinker_of_desc
      : type a wa r. (a, wa, r, t, ApiSpec.wrapped_op) Fun.fn ->
        ((a, wa, r, ApiSpec.wrapped_op) Args.args) QCheck.Shrink.t =
      fun fdesc ->
      let open QCheck in
      match fdesc with
      | Fun.Ret _ty -> Shrink.nil
      | Fun.Ret_or_exc _ty -> Shrink.nil
      | Fun.Ret_ignore_or_exc _ty -> Shrink.nil
      | Fun.Ret_ignore _ty -> Shrink.nil
      | Fun.(Fn (State, fdesc_rem)) ->
        (function (Args.FnState args) ->
           Iter.map (fun args -> Args.FnState args) (gen_shrinker_of_desc fdesc_rem args)
                | _ -> failwith "FnState: should not happen")
      | Fun.(Fn ((Gen (arg_arb,_) | GenDeconstr (arg_arb, _, _)), fdesc_rem)) ->
        (match arg_arb.shrink with
         | None ->
           (function (Args.Fn (a,args)) ->
              Iter.map (fun args -> Args.Fn (a,args)) (gen_shrinker_of_desc fdesc_rem args)
                   | _ -> failwith "Fn/None: should not happen")
         | Some shrk ->
           Iter.(function (Args.Fn (a,args)) ->
               (map (fun a -> Args.Fn (a,args)) (shrk a))
               <+>
               (map (fun args -> Args.Fn (a,args)) (gen_shrinker_of_desc fdesc_rem args))
                        | _ -> failwith "Fn/Some: should not happen"))

    let api =
      List.map (fun (wgt, Elem { name ; fntyp = fdesc ; value = f; wrapped=wrapped_f }) ->
          let rty = ret_type fdesc in
          let open QCheck.Gen in
          (wgt, gen_args_of_desc fdesc >>= fun args ->
           let print = gen_printer name fdesc in
           let shrink = gen_shrinker_of_desc fdesc in
           return (Cmd { name ; args ; rty ; print ; shrink ; f; wrapped_f }))) ApiSpec.api

    let gen_cmd : cmd QCheck.Gen.t = QCheck.Gen.frequency api

    let show_cmd (Cmd { args ; print ; _ }) = print args

    let shrink_cmd (Cmd cmd) =
      QCheck.Iter.map (fun args -> Cmd { cmd with args }) (cmd.shrink cmd.args)

    (* Unsafe if called on two [res] whose internal values are of different
       types. *)
    let equal_res (Res (deconstr, v1)) (Res (_, v2)) =
      match deconstr with
      | Deconstr (_, eq) -> eq v1 (Obj.magic v2)
      | GenDeconstr (_, _, eq) -> eq v1 (Obj.magic v2)

    let show_res (Res (deconstr, value)) =
      match deconstr with
      | Deconstr (print, _) -> print value
      | GenDeconstr (_, print, _) -> print value

    let rec apply_f
      : type a af r. a -> (a,af,r,ApiSpec.wrapped_op) Args.args -> t -> r = fun f args state ->
      match args with
      | Ret _ ->
        f
      | Ret_or_exc _ ->
        (* A constant value in the API cannot raise an exception *)
        raise (Invalid_argument "apply_f")
      | Ret_ignore _ ->
        ()
      | Ret_ignore_or_exc _ ->
        (* A constant value in the API cannot raise an exception *)
        raise (Invalid_argument "apply_f")
      | FnState (Ret _) ->
        f state
      | FnState (Ret_or_exc _) ->
        begin
          try Ok (f state)
          with e -> Error e
        end
      | FnState (Ret_ignore _) ->
        ignore (f state)
      | FnState (Ret_ignore_or_exc _) ->
        begin
          try Ok (ignore @@ f state)
          with e -> Error e
        end
      | FnState (Fn _ as rem) ->
        apply_f (f state) rem state
      | FnState (FnState _ as rem) ->
        apply_f (f state) rem state
      | Fn (arg, Ret _) ->
        f arg
      | Fn (arg, Ret_or_exc _) ->
        begin
          try Ok (f arg)
          with e -> Error e
        end
      | Fn (arg, Ret_ignore _) ->
        ignore @@ f arg
      | Fn (arg, Ret_ignore_or_exc _) ->
        begin
          try Ok (ignore @@ f arg)
          with e -> Error e
        end
      | Fn (arg, (Fn _ as rem)) ->
        apply_f (f arg) rem state
      | Fn (arg, (FnState _ as rem)) ->
        apply_f (f arg) rem state

    let rec apply_wrapped_f
      : type a af r. af -> (a,af,r,ApiSpec.wrapped_op) Args.args -> t -> ApiSpec.wrapped_op = fun f args state ->
      match args with
      | Ret _ ->
        f
      | Ret_or_exc _ ->
        (* A constant value in the API cannot raise an exception *)
        f
      | Ret_ignore _ ->
        f
      | Ret_ignore_or_exc _ ->
        (* A constant value in the API cannot raise an exception *)
        f
      | FnState (Ret _) ->
        f state
      | FnState (Ret_or_exc _) -> f state
      | FnState (Ret_ignore _) -> f state
      | FnState (Ret_ignore_or_exc _) ->
        begin
          f state
        end
      | FnState (Fn _ as rem) ->
        apply_wrapped_f (f state) rem state
      | FnState (FnState _ as rem) ->
        apply_wrapped_f (f state) rem state
      | Fn (arg, Ret _) ->
        f arg
      | Fn (arg, Ret_or_exc _) ->
        begin
          (f arg)
        end
      | Fn (arg, Ret_ignore _) ->
        f arg
      | Fn (arg, Ret_ignore_or_exc _) ->
        begin
          (f arg)        
        end
      | Fn (arg, (Fn _ as rem)) ->
        apply_wrapped_f (f arg) rem state
      | Fn (arg, (FnState _ as rem)) ->
        apply_wrapped_f (f arg) rem state


    let run cmd state =
      let Cmd { args ; rty ; f ; _ } = cmd in
      Res (rty, apply_f f args state)

    let compile pool cmd =
      let state = ApiSpec.init () in
      let Cmd { args ; rty ; wrapped_f ; _ } = cmd in
      let promise, get = Domainslib.Task.promise () in
      let op = apply_wrapped_f (wrapped_f get) args state in
      ApiSpec.cleanup state;
      op, fun () -> Res (rty, Domainslib.Task.await pool promise)

    let run_batched pool t cmds =
      (* retrieve ops for parallel operations *)
      let ops, promises =
        List.map (fun cmd ->
            compile pool cmd
          ) cmds
        |> List.split in
      ApiSpec.run pool t ops;
      let results = List.map (fun (op, f) -> op, f ()) @@ List.combine cmds promises in
      results

  end


  module Make_internal (Spec: Internal.CmdSpec) : sig

    val lin_test : count:int -> name:string -> QCheck2.Test.t

    val neg_lin_test : count:int -> name:string -> QCheck2.Test.t

  end = struct
    module M = Internal.Make(Spec)

    (* setup pool *)
    let num_domains = Domain.recommended_domain_count ()[@alert "-unstable"]
    let pool = Domainslib.Task.setup_pool ~num_domains ()


    let lin_prop (seq_pref, cmds) =
      
       let t = Spec.init () in

      (* run sequential prefix (but use the batching api) *)
      let pref = Domainslib.Task.run pool @@ fun () ->
        List.concat_map (fun cmd ->
            Spec.run_batched pool t [cmd]
          ) seq_pref in
      (* now, run whole batch at once *)
      let obs = Domainslib.Task.run pool @@ fun () -> Spec.run_batched pool t cmds in
      (* search for explanation *)
      M.check_for_sequential_explanation pref obs

    let lin_test ~count ~name =
      M.lin_test ~rep_count:50 ~count ~retries:3 ~name ~lin_prop:lin_prop

    let neg_lin_test ~count ~name =
      M.neg_lin_test ~rep_count:50 ~count ~retries:3 ~name ~lin_prop:lin_prop

  end
end


module Make (Spec: Spec) = Make_internal(MakeCmd(Spec))
