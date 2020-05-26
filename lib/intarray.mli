module IntArray : sig
  module Array : sig
    type t
    val create_uninitialised : int -> t
    val get : t -> int -> int
    val set : t -> int -> int -> unit
    val unsafe_get : t -> int -> int
    val unsafe_set : t -> int -> int -> unit
    val length : t -> int
    val sub : t -> int -> int -> t
    val copy : t -> t 
    val blit : t -> int -> t -> int -> int -> unit
  end
end