open QCheck

(** This is a variant of refs to test for exactly one cleanup per init
    (no double cleanup, no missing cleanup) *)

let cleanup_counter = Atomic.make 0

module RConf =
struct
  exception Already_cleaned
  type status = Inited | Cleaned

  type cmd =
    | Get
    | Set of int
    | Add of int [@@deriving show { with_path = false }]

  type t = (status ref) * (int ref)

  let gen_cmd =
    let int_gen = Gen.nat in
      (Gen.oneof
         [Gen.return Get;
	  Gen.map (fun i -> Set i) int_gen;
	  Gen.map (fun i -> Add i) int_gen;
         ])

  let shrink_cmd = Shrink.nil

  let init () =
    Atomic.incr cleanup_counter ;
    (ref Inited, ref 0)

  let cleanup (status,_) =
    Atomic.decr cleanup_counter ;
    if !status = Cleaned
    then raise Already_cleaned
    else status := Cleaned

  type res = RGet of int | RSet | RAdd [@@deriving show { with_path = false }, eq]

  let run c (_,r) = match c with
    | Get   -> RGet (!r)
    | Set i -> (r:=i; RSet)
    | Add i -> (let old = !r in r := i + old; RAdd) (* buggy: not atomic *)
end

module RT = Lin_domain.Make_internal(RConf) [@alert "-internal"]
;;
Test.check_exn
  (let seq_len,par_len = 20,15 in
   Test.make ~count:1000 ~name:("exactly one-cleanup test")
     (RT.arb_cmds_triple seq_len par_len)
     (fun input ->
        try
          ignore (RT.lin_prop input);
          Atomic.get cleanup_counter = 0
        with
        | RConf.Already_cleaned -> failwith "Already cleaned"
        | _ -> Atomic.get cleanup_counter = 0
     ))
