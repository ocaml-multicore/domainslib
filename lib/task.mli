type 'a task = unit -> 'a
(** Type of task *)

type 'a promise

type pool
(** Type of task pool *)

val setup_pool : num_domains:int -> pool
(** Sets up a task execution pool with [num_domains]. *)

val teardown_pool : pool -> unit
(** Tears down the task execution pool. *)

val async : pool -> 'a task -> 'a promise
(** [async p t] runs the task [t] asynchronously in the pool [p]. The function
  * returns a promise [r] in which the result of the task [t] will be stored.
  * *)

val await : pool -> 'a promise -> 'a
(** [await p r] waits for the promise to be resolved. If the task associated
 * with the promise had completed sucessfully, then the result of the task will
 * be returned. If the task had raised an exception, then [await] raises the
 * same exception. *)

val parallel_for : pool -> chunk_size:int -> start:int -> finish:int ->
                   body:(int -> unit) -> unit
(** [parallel_for p c s f b] behaves similar to [for i=s to f do b i done], but
  * runs the for loop in parallel. The chunk size [c] determines the
  * granularity of parallelisation. Individual iterates may be run in any
  * order. *)

val parallel_for_reduce : pool -> ('a -> 'a -> 'a) -> 'a -> chunk_size:int ->
                          start:int -> finish:int -> body:(int -> 'a) -> 'a
(** [parallel_for_reduce p r i c s f b] is similar to [parallel_for] except
  * that the result returned by each iteration is reduced with [r] with initial
  * value [i]. *)
