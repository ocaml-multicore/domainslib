module Int64Array : sig
  module Array : sig
    type t
    val create_uninitialised : int -> t
    val get : t -> int -> int64
    val set : t -> int -> int64 -> unit
    val unsafe_get : t -> int -> int64
    val unsafe_set : t -> int -> int64 -> unit
    val unsafe_blit : t -> int -> t -> int -> int -> unit
    val length : t -> int
    val sub : t -> int -> int -> t
    val copy : t -> t 
  end
end = struct
  module Array = struct
    type t = Bytes.t
    let create_uninitialised sz = Bytes.create (sz * 8)
    external set_raw_64 : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64"
    external get_raw_64 : Bytes.t -> int -> int64 = "%caml_bytes_get64"
    external unsafe_set_raw_64 : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64u"
    external unsafe_get_raw_64 : Bytes.t -> int -> int64 = "%caml_bytes_get64u"
    external unsafe_blit : Bytes.t -> int -> Bytes.t -> int -> int -> unit
                     = "caml_blit_bytes" [@@noalloc]
    let get arr i = get_raw_64 arr (i * 8)
    let set arr i x = set_raw_64 arr (i * 8) x
    let unsafe_get arr i = unsafe_get_raw_64 arr (i * 8)
    let unsafe_set arr i x = unsafe_set_raw_64 arr (i * 8) x
    let length a = (Bytes.length a) / 8
    let sub a s len = Bytes.sub a (s * 8) (len * 8)
    let copy = Bytes.copy
  end
end

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
end = struct
  module Array = struct
    type t = Int64Array.Array.t
    let create_uninitialised = Int64Array.Array.create_uninitialised
    let get arr i = Int64.to_int (Int64Array.Array.get arr i)
    let set arr i x = Int64Array.Array.set arr i (Int64.of_int x)
    let unsafe_get arr i = Int64.to_int (Int64Array.Array.unsafe_get arr i)
    let unsafe_set arr i x = Int64Array.Array.unsafe_set arr i (Int64.of_int x)
    let length = Int64Array.Array.length
    let sub = Int64Array.Array.sub
    let copy = Int64Array.Array.copy
    let blit s1 ofs1 s2 ofs2 len =
      if len < 0 || ofs1 < 0 || ofs1 > length s1 - len
             || ofs2 < 0 || ofs2 > length s2 - len
      then invalid_arg "Array.blit"
      else Int64Array.Array.unsafe_blit s1 (ofs1 * 8) s2 (ofs2 * 8) (len * 8)
  end
end
  


  