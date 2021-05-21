
type 'a pointer = Null | Ptr of 'a

type 'a node = {
	next: 'a node pointer Atomic.t;
  value: 'a;
}

type 'a t = {
  mutable head: 'a node;
  mutable tail: 'a node;
}

(* store release with the appropriate compiler & hardware barrier *)
let store_release addr value =
  Atomic.set addr value

(* load acquire with the appropriate compiler & hardware barrier *)
let load_acquire addr =
  Atomic.get addr

let make () =
  let n = {next=Atomic.make Null; value=Obj.magic ()} in
  {head=n; tail=n}

let push q v =
  let n = {next=Atomic.make Null; value=v} in
  store_release (q.head).next (Ptr n);
  q.head <- n

let poll q =
  match load_acquire q.tail.next with
  | Null -> None
  | Ptr n ->
    let v = n.value in
    q.tail <- n;
    Some v

let rec pop q =
  match poll q with
  | None -> (Domain.Sync.poll (); pop q)
  | Some v -> v


let _ =
  let n_iter = 1_000_000 in
  let q = make () in
  let producer q =
    for i = 0 to n_iter do
      push q i
    done
  in
  let consumer q =
    let i = ref 0 in
    while !i < n_iter do
      assert((pop q) = !i);
      incr i;
    done
  in
  let prod = Domain.spawn(fun () -> consumer q) in
  producer q;
  Domain.join prod;

  Printf.printf "OK!\n"
