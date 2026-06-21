# sml-msgpack

MessagePack binary serialization (spec v2.0) in pure Standard ML

## Installation

```
smlpkg add github.com/sjqtentacles/sml-msgpack
smlpkg sync
```

## Usage

```sml
(* Build a one-byte string from an int 0-255 *)
fun b (n : int) : string = String.str (Char.chr n)
fun bs (ns : int list) : string = String.concat (List.map b ns)

(* Encode values to the MessagePack wire format *)
val () = print (Msgpack.encode Msgpack.Nil)                 (* 0xc0 *)
val () = print (Msgpack.encode (Msgpack.Bool true))         (* 0xc3 *)
val () = print (Msgpack.encode (Msgpack.Int (IntInf.fromInt 256)))
                                                            (* 0xcd 0x01 0x00 *)
val () = print (Msgpack.encode (Msgpack.Str "hello"))       (* 0xa5 hello *)

(* The smallest encoding family is always chosen automatically *)
val arr = Msgpack.Array [ Msgpack.Int (IntInf.fromInt 1)
                        , Msgpack.Int (IntInf.fromInt 2)
                        , Msgpack.Int (IntInf.fromInt 3) ]
val () = print (Msgpack.encode arr)                         (* 0x93 0x01 0x02 0x03 *)

val m = Msgpack.Map [(Msgpack.Str "a", Msgpack.Int (IntInf.fromInt 1))]
val () = print (Msgpack.encode m)                           (* 0x81 0xa1 0x61 0x01 *)

(* Floats are always encoded as IEEE 754 float64 (0xcb) *)
val () = print (Msgpack.encode (Msgpack.Float 3.14))

(* Binary blobs and extension types *)
val () = print (Msgpack.encode (Msgpack.Bin (bs [0xde, 0xad])))
val () = print (Msgpack.encode (Msgpack.Ext (1, b 0x02)))

(* Decode raises Fail on malformed/truncated input *)
val decoded = Msgpack.decode (bs [0x93, 0x01, 0x02, 0x03])
(* val decoded = Array [Int 1, Int 2, Int 3] *)
```

The data model:

```sml
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
```

`encode : t -> string` serializes a value, choosing the smallest format
family for each value (fixint/uint/int families, fixstr/str8/16/32,
bin8/16/32, fixarray/array16/32, fixmap/map16/32, fixext/ext8/16/32).
`decode : string -> t` parses a value and raises `Fail` on malformed input.
All multibyte integers are big-endian per the MessagePack specification.

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
