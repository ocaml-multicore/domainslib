open Domainslib.IntArray

let () = 
  let a = Array.create_uninitialised 10 in
  for i = 0 to 9 do 
    a.(i) <- i
  done;
  let b = Array.copy a in
  for i = 0 to (Array.length b - 1) do 
    assert (a.(i) = b.(i))
  done;
  let c = Array.create_uninitialised 3 in
  Array.blit a 0 c 0 3;
  assert (c.(0) = a.(0));
  assert (c.(1) = a.(1));
  assert (c.(2) = a.(2));
  Printf.printf "ok"