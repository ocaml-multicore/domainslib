type 'a task = unit -> 'a
(** Type of task *)

type 'a promise
(** Type of promises *)

type pool
(** Type of task pool *)

val setup_pool : ?name:string -> num_additional_domains:int -> unit -> pool
(** Sets up a task execution pool with [num_additional_domains + 1] domains
  * including the current domain. If [name] is provided, the pool is mapped to
  * [name] which can be looked up later with [lookup_pool name].
  * Raises [Invalid_argumet] when [num_additional_domains] is less than 0. *)

exception TasksActive

val teardown_pool : pool -> unit
(** Tears down the task execution pool.
  * Raises [TasksActive] exception if any tasks are currently active. *)

val lookup_pool : string -> pool
(** [lookup_pool name] returns the pool associated to [name].
  * Raises [Not_found] if no pool is associated to [name] *)

val async : pool -> 'a task -> 'a promise
(** [async p t] runs the task [t] asynchronously in the pool [p]. The function
  * returns a promise [r] in which the result of the task [t] will be stored.
  * *)

val await : pool -> 'a promise -> 'a
(** [await p r] waits for the promise to be resolved. If the task associated
 * with the promise had completed sucessfully, then the result of the task will
 * be returned. If the task had raised an exception, then [await] raises the
 * same exception. *)

val parallel_for : ?chunk_size:int -> start:int -> finish:int ->
                   body:(int -> unit) -> pool -> unit
(** [parallel_for c s f b p] behaves similar to [for i=s to f do b i done], but
  * runs the for loop in parallel. The chunk size [c] determines the number of
  * body applications done in one task; this will default to
  * [max(1, (finish-start + 1) / (8 * num_domains))]. Individual iterates may
  * be run in any order. Tasks are distributed to workers using a
  * divide-and-conquer scheme.
  *)

val parallel_for_reduce : ?chunk_size:int -> start:int -> finish:int ->
                body:(int -> 'a) -> pool -> ('a -> 'a -> 'a) -> 'a -> 'a
(** [parallel_for_reduce c s f b p r i] is similar to [parallel_for] except
  * that the result returned by each iteration is reduced with [r] with initial
  * value [i]. The reduce operations are performed in an arbitrary order and the
  * reduce function needs to be commutative and associative in order to obtain 
  * a deterministic result. *)

val parallel_scan : pool -> ('a -> 'a -> 'a) -> 'a array -> 'a array
(** [parallel_scan p op a] computes the scan of the array [a]
  * in parallel with binary operator [op] and returns the result array.
  * Scan is similar to [Array.fold_left] but returns an array of reduced
  * intermediate values. The reduce operations are performed in an arbitrary
  * order and the reduce function needs to be commutative and associative in 
  * order to obtain a deterministic result *)
