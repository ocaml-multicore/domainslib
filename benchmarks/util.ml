module IntSet = Set.Make(Int)

let gen_random_array ~min ~max count =
  if max - min < count then failwith "Not enough unique values";
  let seen_ints = ref IntSet.empty in
  let rec fresh_int s =
    let vl = min + Random.int (max - min) in
    if IntSet.mem vl !seen_ints
    then fresh_int s
    else (seen_ints := IntSet.add vl !seen_ints; vl) in
  Array.init count fresh_int