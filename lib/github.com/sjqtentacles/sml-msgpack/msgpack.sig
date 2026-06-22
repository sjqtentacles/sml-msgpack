(* msgpack.sig — MessagePack (spec v2.0) encoder/decoder contract *)
signature MSGPACK =
sig
  datatype t
    = Nil
    | Bool   of bool
    | Int    of IntInf.int
    | Float  of real
    | Str    of string
    | Bin    of string
    | Array  of t list
    | Map    of (t * t) list
    | Ext    of int * string
  val encode : t -> string
  val decode : string -> t        (* raises Fail on malformed input *)

  (* Canonical encoding: the minimal MessagePack encoding (same byte choices
     as `encode`, which already picks the shortest format family) with map
     keys emitted in ascending bytewise lexicographic order of their own
     canonical-encoded key bytes (plain byte-string comparison: byte by byte,
     a shorter prefix sorting first). Applied recursively to nested maps and
     arrays, so equal values always produce identical bytes regardless of the
     order map keys were supplied in. *)
  val encodeCanonical : t -> string
end
