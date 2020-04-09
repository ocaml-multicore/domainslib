type 'a task = unit -> 'a
(** Type of task *)

type ('a,'b) reducer
(** Type of reducer *)

type pool
(** Type of task pool *)

val setup_pool : num_domains:int -> pool
(** Sets up a task execution pool with [num_domains]. *)

val teardown_pool : pool -> unit
(** Tears down the task execution pool. *)

val parallel_for : pool -> chunk_size:int -> start:int -> finish:int ->
                   body:(int -> unit) -> unit
(** [parallel_for p c s f b] behaves similar to [for i=s to f do b i done], but
 * runs the for loop in parallel. The chunk size [c] determines the granularity
 * of parallelisation. Individual iterates may be run in any order. *)

val parallel_for_reduce : pool -> ('a -> 'a -> 'a) -> 'a -> chunk_size:int ->
                          start:int -> finish:int -> body:(int -> 'a) -> 'a
(** [parallel_for_reduce p r i c s f b] is similar to [parallel_for] except
 * that the result returned by each iteration is reduced with [r] with initial
 * value [i]. *)

val make_reducer : ('a -> 'b -> 'a) -> 'a -> ('a, 'b) reducer
(** [make_reducer reduce init] creates a new reducer that can be used to gather
 * the results of parallel tasks using the reduction function [reduce] with
 * initial value [init]. *)

val add_task : pool -> ('a,'b) reducer -> 'b task -> unit
(** [add_task p r t] adds the task [t] to the pool [p] whose results will be
 * gathered by the reducer [r].
 *
 * The results of tasks added to already reduced pool will be lost. *)

val reduce : pool -> ('a,'b) reducer -> 'a
(** [reduce p r] gathers the result from all the tasks that were spawned in the
 * context of this reducer and returns the result.
 *
 * Reduce must be called exactly once after all the tasks have been added to
 * this reducer. Raises [Invalid_argument] if [r] has already been reduced. *)


