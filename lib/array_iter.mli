
val parallel_iter : ('a -> unit) -> 'a array -> Task.pool -> unit
(** [parallel_iter f a pool] applies function [f] in turn to all the elements of [a]
    [pool] is the Task Pool
    It does Stdlib.Array.iter in parallel *)

val parallel_iteri : (int -> 'a -> unit) -> 'a array -> Task.pool -> unit
(** Same as {!parallel_iter}, but the function is applied to the index of the element as first 
    argument, and the element itself as second argument
    It performs Stdlib.Array.iteri in parallel *)