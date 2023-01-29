
type 'a tree = 
  | Node of {
      keys : 'a option array;
      n : int Atomic.t;
      child : 'a tree array;
      size : int Atomic.t
    } 
  | Leaf

type 'a t = {
  mutable root : 'a tree;
  degree : int
}
let create ~degree = {root = Leaf; degree}
let make_node degree = 
  Node {
    keys = Array.make (degree-1) None; 
    n = Atomic.make 0; 
    child = Array.make degree Leaf; 
    size = Atomic.make 0
  }

let split_child node idx = failwith ""


let insert_non_full node key = failwith ""

let insert t key = 
  match t.root with
  (* Empty *)
  | Leaf -> 
    let root = make_node t.degree in
    failwith ""

  | Node x -> failwith ""



let%test "basic test" = false
let%test "search test" = false
let%test "insert test" = false
let%test "delete test" = false