module T = Domainslib.Task

let rec foo i =
  if i = 0 then ()
  else begin
    ignore (failwith "exn");
    foo i
  end
  [@@inline never]

let rec bar i =
  if i = 0 then ()
  else begin
    foo i;
    bar i
  end
  [@@inline never]

let main () =
  let pool = T.setup_pool ~num_additional_domains:0 () in
  T.run pool (fun _ ->
    let p = T.async pool (fun _ -> bar 42) in
    T.await pool p;
    Printf.printf "should not reach here\n%!");
  T.teardown_pool pool

let _ =
  try main ()
  with _ ->
    let open Printexc in
    let bt = get_raw_backtrace () in
    let bt_slot_arr = Option.get (backtrace_slots bt) in
    assert (Option.get (Slot.name bt_slot_arr.(1)) = "Backtrace.foo");
    let s = raw_backtrace_to_string bt in
    print_string s
