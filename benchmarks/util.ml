module IntSet = Set.Make(Int)

let max_rdm_int = (Int.shift_left 1 30) - 1
let gen_random_array ~min ~max count =
  match () with
  | _ when max - min < count -> failwith "if min < 0 then failwith "
  | _ when min < 0 -> failwith "min cannot be less than 0"
  | _ when max > max_rdm_int ->  
    let s = Format.sprintf "max cannot be greater than %d" max_rdm_int in failwith s
  | _ -> ();
    let seen_ints = ref IntSet.empty in
    let rec fresh_int s =
      let vl = min + Random.int (max - min) in
      if IntSet.mem vl !seen_ints
      then fresh_int s
      else (seen_ints := IntSet.add vl !seen_ints; vl) in
    Array.init count fresh_int