let rec log2 n = if n <= 1 then 0 else 1 + (log2 (n asr 1))
let chunk_calculator ~batch_size ~operations () =
  let chunk = operations/batch_size in
  if chunk <= 0 then 1 else 
    (let n = log2 chunk + 1 in
     Int.shift_left 1 n)

let print_implicit_batch_stats stats = 
  let stat_len = Hashtbl.length stats in
  let stats_array = Array.make stat_len 0 in
  let i = ref 0 in
  Hashtbl.iter (fun x _ -> stats_array.(!i) <- x; incr i) stats;
  Array.sort (Int.compare) stats_array;
  Array.iter (fun x ->
      let value = Hashtbl.find stats x in
      Printf.printf "%d -> %d\n" x value) stats_array;