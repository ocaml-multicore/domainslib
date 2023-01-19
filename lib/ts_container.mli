type 'a t
val create : ?batch_limit:int -> unit -> 'a t
val add : 'a t -> 'a -> unit
val get : 'a t -> 'a array * int
val size : 'a t -> int