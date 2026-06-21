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
end
