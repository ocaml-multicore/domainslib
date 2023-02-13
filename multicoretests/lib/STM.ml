(** Base module for specifying STM tests *)

open QCheck

type 'a ty = ..

type _ ty +=
  | Unit : unit ty
  | Bool : bool ty
  | Char : char ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Float : float ty
  | String : string ty
  | Bytes : bytes ty
  | Exn : exn ty
  | Option : 'a ty -> 'a option ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty
  | List : 'a ty -> 'a list ty
  | Array : 'a ty -> 'a array ty
  | Seq : 'a ty -> 'a Seq.t ty

type 'a ty_show = 'a ty * ('a -> string)

let unit = (Unit, fun () -> "()")
let bool = (Bool, string_of_bool)
let char = (Char, fun c -> Printf.sprintf "%C" c)
let int = (Int, string_of_int)
let int32 = (Int32, Int32.to_string)
let int64 = (Int64, Int64.to_string)
let float = (Float, Float.to_string)
let string = (String, fun s -> Printf.sprintf "%S" s)
let bytes = (Bytes, fun b -> QCheck.Print.string (Bytes.to_string b))
let option spec =
  let (ty,show) = spec in
  (Option ty, QCheck.Print.option show)
let exn = (Exn, Printexc.to_string)

let show_result show_ok show_err = function
  | Ok x    -> Printf.sprintf "Ok (%s)" (show_ok x)
  | Error y -> Printf.sprintf "Error (%s)" (show_err y)

let result spec_ok spec_err =
  let (ty_ok, show_ok) = spec_ok in
  let (ty_err, show_err) = spec_err in
  (Result (ty_ok, ty_err), show_result show_ok show_err)
let list spec =
  let (ty,show) = spec in
  (List ty, QCheck.Print.list show)
let array spec =
  let (ty,show) = spec in
  (Array ty, QCheck.Print.array show)
let seq spec =
  let (ty,show) = spec in
  (Seq ty, fun s -> QCheck.Print.list show (List.of_seq s))



type res =
    Res : 'a ty_show * 'a -> res

let show_res (Res ((_,show), v)) = show v

(** The specification of a state machine. *)
module type Spec =
sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state *)

  type sut
  (** The type of the system under test *)

  val arb_cmd : state -> cmd arbitrary
  (** A command generator. Accepts a state parameter to enable state-dependent [cmd] generation. *)

  val init_state : state
  (** The model's initial state. *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  val next_state : cmd -> state -> state
  (** Move the internal state machine to the next state. *)

  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the [sut] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c].
      This is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)

  val run : cmd -> sut -> res
  (** [run c i] should interpret the command [c] over the system under test (typically side-effecting). *)

  val postcond : cmd -> state -> res -> bool
  (** [postcond c s res] checks whether [res] arising from interpreting the
      command [c] over the system under test with [run] agrees with the
      model's result.
      Note: [s] is in this case the model's state prior to command execution. *)
end


module Internal =
struct
  module Make(Spec : Spec) = struct

    let rec gen_cmds arb s fuel =
      Gen.(if fuel = 0
           then return []
           else
             (arb s).gen >>= fun c ->
             (gen_cmds arb (Spec.next_state c s) (fuel-1)) >>= fun cs ->
             return (c::cs))
    (** A fueled command list generator.
        Accepts a state parameter to enable state-dependent [cmd] generation. *)

    let rec cmds_ok s cs = match cs with
      | [] -> true
      | c::cs ->
        Spec.precond c s &&
        let s' = Spec.next_state c s in
        cmds_ok s' cs

    (* This is an adaption of [QCheck.Shrink.list_spine]
       with more base cases added *)
    let rec shrink_list_spine l yield =
      let rec split l len acc = match len,l with
        | _,[]
        | 0,_ -> List.rev acc, l
        | _,x::xs -> split xs (len-1) (x::acc) in
      match l with
      | [] -> ()
      | [_] -> yield []
      | [x;y] -> yield []; yield [x]; yield [y]
      | [x;y;z] -> yield [x]; yield [x;y]; yield [x;z]; yield [y;z]
      | [x;y;z;w] -> yield [x;y;z]; yield [x;y;w]; yield [x;z;w]; yield [y;z;w]
      | _::_ ->
        let len = List.length l in
        let xs,ys = split l ((1 + len) / 2) [] in
        yield xs;
        shrink_list_spine xs (fun xs' -> yield (xs'@ys))

    (* This is an adaption of [QCheck.Shrink.list] *)
    let shrink_list ?shrink l yield =
      shrink_list_spine l yield;
      match shrink with
      | None -> () (* no elem. shrinker provided *)
      | Some shrink -> Shrink.list_elems shrink l yield

    let arb_cmds s =
      let cmds_gen = Gen.sized (gen_cmds Spec.arb_cmd s) in
      let shrinker = shrink_list ?shrink:(Spec.arb_cmd s).shrink in (* pass opt. elem. shrinker *)
      let ac = QCheck.make ~shrink:(Shrink.filter (cmds_ok Spec.init_state) shrinker) cmds_gen in
      (match (Spec.arb_cmd s).print with
       | None   -> ac
       | Some p -> set_print (Util.print_vertical p) ac)

    let consistency_test ~count ~name =
      Test.make ~name ~count (arb_cmds Spec.init_state) (cmds_ok Spec.init_state)

    let rec interp_agree s sut cs = match cs with
      | [] -> true
      | c::cs ->
        let res = Spec.run c sut in
        let b   = Spec.postcond c s res in
        let s'  = Spec.next_state c s in
        b && interp_agree s' sut cs

    let rec check_disagree s sut cs = match cs with
      | [] -> None
      | c::cs ->
        let res = Spec.run c sut in
        let b   = Spec.postcond c s res in
        let s'  = Spec.next_state c s in
        if b
        then
          match check_disagree s' sut cs with
          | None -> None
          | Some rest -> Some ((c,res)::rest)
        else Some [c,res]

    let check_and_next (c,res) s =
      let b  = Spec.postcond c s res in
      let s' = Spec.next_state c s in
      b,s'

    (* checks that all interleavings of a cmd triple satisfies all preconditions *)
    let rec all_interleavings_ok pref cs1 cs2 s =
      match pref with
      | c::pref' ->
        Spec.precond c s &&
        let s' = Spec.next_state c s in
        all_interleavings_ok pref' cs1 cs2 s'
      | [] ->
        match cs1,cs2 with
        | [],[] -> true
        | [],c2::cs2' ->
          Spec.precond c2 s &&
          let s' = Spec.next_state c2 s in
          all_interleavings_ok pref cs1 cs2' s'
        | c1::cs1',[] ->
          Spec.precond c1 s &&
          let s' = Spec.next_state c1 s in
          all_interleavings_ok pref cs1' cs2 s'
        | c1::cs1',c2::cs2' ->
          (Spec.precond c1 s &&
           let s' = Spec.next_state c1 s in
           all_interleavings_ok pref cs1' cs2 s')
          &&
          (Spec.precond c2 s &&
           let s' = Spec.next_state c2 s in
           all_interleavings_ok pref cs1 cs2' s')

    let rec check_obs pref cs1 cs2 s =
      match pref with
      | p::pref' ->
        let b,s' = check_and_next p s in
        b && check_obs pref' cs1 cs2 s'
      | [] ->
        match cs1,cs2 with
        | [],[] -> true
        | [],p2::cs2' ->
          let b,s' = check_and_next p2 s in
          b && check_obs pref cs1 cs2' s'
        | p1::cs1',[] ->
          let b,s' = check_and_next p1 s in
          b && check_obs pref cs1' cs2 s'
        | p1::cs1',p2::cs2' ->
          (let b1,s' = check_and_next p1 s in
           b1 && check_obs pref cs1' cs2 s')
          ||
          (let b2,s' = check_and_next p2 s in
           b2 && check_obs pref cs1 cs2' s')

    let gen_cmds_size gen s size_gen = Gen.sized_size size_gen (gen_cmds gen s)

    (* Shrinks a single cmd, starting in the given state *)
    let shrink_cmd arb cmd state =
      Option.value (arb state).shrink ~default:Shrink.nil @@ cmd

    (* Shrinks cmd list elements, starting in the given state *)
    let rec shrink_cmd_list_elems arb cs state = match cs with
      | [] -> Iter.empty
      | c::cs ->
        if Spec.precond c state
        then
          Iter.(
            map (fun c -> c::cs) (shrink_cmd arb c state)
            <+>
            map (fun cs -> c::cs) (shrink_cmd_list_elems arb cs Spec.(next_state c state))
          )
        else Iter.empty

    (* Shrinks cmd elements in triples *)
    let shrink_triple_elems arb0 arb1 arb2 (seq,p1,p2) =
      let shrink_prefix_elems cs state =
        Iter.map (fun cs -> (cs,p1,p2)) (shrink_cmd_list_elems arb0 cs state)
      in
      let rec shrink_par_suffix_elems cs state = match cs with
        | [] ->
          (* try only one option: p1s or p2s first - both valid interleavings *)
          Iter.(map (fun p1 -> (seq,p1,p2)) (shrink_cmd_list_elems arb1 p1 state)
                <+>
                map (fun p2 -> (seq,p1,p2)) (shrink_cmd_list_elems arb2 p2 state))
        | c::cs ->
          (* walk seq prefix (again) to advance state *)
          if Spec.precond c state
          then shrink_par_suffix_elems cs Spec.(next_state c state)
          else Iter.empty
      in
      match Spec.(arb_cmd init_state).shrink with
      | None -> Iter.empty (* stop early if no cmd shrinker is available *)
      | Some _ ->
        Iter.(shrink_prefix_elems seq Spec.init_state
              <+>
              shrink_par_suffix_elems seq Spec.init_state)

    (* General shrinker of cmd triples *)
    let shrink_triple arb0 arb1 arb2 =
      let open Iter in
      Shrink.filter
        (fun (seq,p1,p2) -> all_interleavings_ok seq p1 p2 Spec.init_state)
        (fun ((seq,p1,p2) as triple) ->
           (* Shrinking heuristic:
              First reduce the cmd list sizes as much as possible, since the interleaving
              is most costly over long cmd lists. *)
           (map (fun seq' -> (seq',p1,p2)) (shrink_list_spine seq))
           <+>
           (fun yield -> (match p1 with [] -> Iter.empty | c1::c1s -> Iter.return (seq@[c1],c1s,p2)) yield)
           <+>
           (fun yield -> (match p2 with [] -> Iter.empty | c2::c2s -> Iter.return (seq@[c2],p1,c2s)) yield)
           <+>
           (map (fun p1' -> (seq,p1',p2)) (shrink_list_spine p1))
           <+>
           (map (fun p2' -> (seq,p1,p2')) (shrink_list_spine p2))
           <+>
           (* Secondly reduce the cmd data of individual list elements *)
           (shrink_triple_elems arb0 arb1 arb2 triple))

    let arb_triple seq_len par_len arb0 arb1 arb2 =
      let seq_pref_gen = gen_cmds_size arb0 Spec.init_state (Gen.int_bound seq_len) in
      let shrink_triple = shrink_triple arb0 arb1 arb2 in
      let gen_triple =
        Gen.(seq_pref_gen >>= fun seq_pref ->
             int_range 2 (2*par_len) >>= fun dbl_plen ->
             let spawn_state = List.fold_left (fun st c -> Spec.next_state c st) Spec.init_state seq_pref in
             let par_len1 = dbl_plen/2 in
             let par_gen1 = gen_cmds_size arb1 spawn_state (return par_len1) in
             let par_gen2 = gen_cmds_size arb2 spawn_state (return (dbl_plen - par_len1)) in
             triple (return seq_pref) par_gen1 par_gen2) in
      make ~print:(Util.print_triple_vertical Spec.show_cmd) ~shrink:shrink_triple gen_triple

    let arb_cmds_triple seq_len par_len = arb_triple seq_len par_len Spec.arb_cmd Spec.arb_cmd Spec.arb_cmd
  end
end

include Util
