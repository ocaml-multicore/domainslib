let inserts = 10_000_000
let main () =
  let t = Btree.create () in
  for i = 1 to inserts do
    let value = "Key" ^ string_of_int i in
    Btree.insert t i value
  done;
  assert (Btree.search t (inserts/2) |> Option.is_some)

let () = Utils.time main