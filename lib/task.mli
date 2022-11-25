type 'a task = unit -> 'a
(** Type of task *)

type !'a promise
(** Type of promises *)

type pool
(** Type of task pool *)

val setup_pool : ?name:string -> num_domains:int -> unit -> pool
(** Sets up a task execution pool with [num_domains] new domains. If [name] is
    provided, the pool is mapped to [name] which can be looked up later with
    [lookup_pool name].

    When [num_domains] is 0, the new pool will be empty, and when an empty
    pool is in use, every function in this module will run effectively
    sequentially, using the calling domain as the only available domain.

    Raises {!Invalid_argument} when [num_domains] is less than 0. *)

val teardown_pool : pool -> unit
(** Tears down the task execution pool. *)

val lookup_pool : string -> pool option
(** [lookup_pool name] returns [Some pool] if [pool] is associated to [name] or
    returns [None] if no value is associated to it. *)

val get_num_domains : pool -> int
(** [get_num_domains pool] returns the total number of domains in [pool]
    including the parent domain. *)

val run : pool -> 'a task -> 'a
(** [run p t] runs the task [t] synchronously with the calling domain and the
    domains in the pool [p]. If the task [t] blocks on a promise, then tasks
    from the pool [p] are executed until the promise blocking [t] is resolved.

    This function should be used at the top level to enclose the calls to other
    functions that may await on promises. This includes {!await},
    {!parallel_for} and its variants. Otherwise, those functions will raise
    [Unhandled] exception. *)

val async : pool -> 'a task -> 'a promise
(** [async p t] runs the task [t] asynchronously in the pool [p]. The function
    returns a promise [r] in which the result of the task [t] will be stored.
  *)

val await : pool -> 'a promise -> 'a
(** [await p r] waits for the promise [r] to be resolved. During the resolution,
    other tasks in the pool [p] might be run using the calling domain and/or the
    domains in the pool [p]. If the task associated with the promise have
    completed successfully, then the result of the task will be returned. If the
    task have raised an exception, then [await] raises the same exception.

    Must be called with a call to {!run} in the dynamic scope to handle the
    internal algebraic effects for task synchronization. *)

val promise : unit -> 'a promise * ('a -> unit)
(** [promise ()] returns a new promise and a function to resolve it
    to a value. The function can only be called once. *)

val parallel_for : ?chunk_size:int -> start:int -> finish:int ->
                   body:(int -> unit) -> pool -> unit
(** [parallel_for c s f b p] behaves similar to [for i=s to f do b i done], but
    runs the for loop in parallel with the calling domain and/or the domains in
    the pool [p]. The chunk size [c] determines the number of body applications
    done in one task; this will default to [max(1, (finish-start + 1) / (8 *
    num_domains))]. Individual iterations may be run in any order. Tasks are
    distributed to the participating domains using a divide-and-conquer scheme.

    Must be called with a call to {!run} in the dynamic scope to handle the
    internal algebraic effects for task synchronization. *)

val parallel_for_reduce : ?chunk_size:int -> start:int -> finish:int ->
                body:(int -> 'a) -> pool -> ('a -> 'a -> 'a) -> 'a -> 'a
(** [parallel_for_reduce c s f b p r i] is similar to [parallel_for] except
    that the result returned by each iteration is reduced with [r] with initial
    value [i]. The reduce operations are performed in an arbitrary order and
    the reduce function needs to be associative in order to obtain a
    deterministic result.

    Must be called with a call to {!run} in the dynamic scope to handle the
    internal algebraic effects for task synchronization. *)

val parallel_scan : pool -> ('a -> 'a -> 'a) -> 'a array -> 'a array
(** [parallel_scan p op a] computes the scan of the array [a] in parallel with
    binary operator [op] and returns the result array, using the calling domain
    and/or the domains in the pool [p]. Scan is similar to [Array.fold_left]
    but returns an array of reduced intermediate values. The reduce operations
    are performed in an arbitrary order and the reduce function needs to be
    associative in order to obtain a deterministic result.

    Must be called with a call to {!run} in the dynamic scope to handle the
    internal algebraic effects for task synchronization. *)
