(* msgpack.sml — MessagePack (spec v2.0) encoder/decoder implementation *)
structure Msgpack :> MSGPACK =
struct

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

  (* ------------------------------------------------------------------ *)
  (* Helpers                                                              *)
  (* ------------------------------------------------------------------ *)

  val zero = IntInf.fromInt 0
  val one  = IntInf.fromInt 1
  val i256 = IntInf.fromInt 256

  fun byteStr (n : IntInf.int) : string =
    String.str (Char.chr (IntInf.toInt (IntInf.mod (n, i256))))

  (* Big-endian encode a non-negative IntInf into exactly k bytes *)
  fun beBytes (k : int) (v : IntInf.int) : string =
    let
      fun go (0, _, acc) = acc
        | go (i, cur, acc) =
            go (i - 1, IntInf.div (cur, i256), byteStr cur ^ acc)
    in go (k, v, "") end

  (* ------------------------------------------------------------------ *)
  (* Float64 (IEEE 754 double) encode/decode — manual, big-endian.       *)
  (* Works on both MLton and Poly/ML.                                    *)
  (* ------------------------------------------------------------------ *)

  val pow16 = IntInf.fromInt 65536
  val pow32 = pow16 * pow16
  val pow48 = pow32 * pow16
  val pow52 = pow32 * IntInf.fromInt 1048576
  val pow56 = pow48 * IntInf.fromInt 256
  val pow31 = pow16 * IntInf.fromInt 32768           (* 2^31 *)
  val pow64 = pow32 * pow32                          (* 2^64 *)

  fun encodeFloat64 (r : real) : string =
    let
      val signBit : IntInf.int = if Real.signBit r then one else zero

      fun bits () : IntInf.int =
        if Real.isNan r then
          IntInf.fromInt 2047 * pow52 + IntInf.div (pow52, IntInf.fromInt 2)
        else if not (Real.isFinite r) then
          (signBit * IntInf.fromInt 2048 + IntInf.fromInt 2047) * pow52
        else if Real.== (Real.abs r, 0.0) then
          signBit * IntInf.fromInt 2048 * pow52
        else
          let
            val {man = m, exp = e} = Real.toManExp r
            val storedExp = e - 1 + 1023
            val fracR = Real.abs m * 2.0 - 1.0
            val fracI : IntInf.int =
              Real.toLargeInt IEEEReal.TO_ZERO
                (fracR * Real.fromLargeInt pow52)
          in
            signBit * IntInf.fromInt 2048 * pow52 +
            IntInf.fromInt storedExp * pow52 +
            fracI
          end
    in
      beBytes 8 (bits ())
    end

  (* ------------------------------------------------------------------ *)
  (* Encoder                                                              *)
  (* ------------------------------------------------------------------ *)

  fun encodeInt (n : IntInf.int) : string =
    if n >= zero then
      (* unsigned families *)
      if n <= IntInf.fromInt 127 then
        byteStr n                                    (* positive fixint *)
      else if n <= IntInf.fromInt 255 then
        byteStr (IntInf.fromInt 0xcc) ^ beBytes 1 n  (* uint8 *)
      else if n <= IntInf.fromInt 65535 then
        byteStr (IntInf.fromInt 0xcd) ^ beBytes 2 n  (* uint16 *)
      else if n <= pow32 - one then
        byteStr (IntInf.fromInt 0xce) ^ beBytes 4 n  (* uint32 *)
      else
        byteStr (IntInf.fromInt 0xcf) ^ beBytes 8 n  (* uint64 *)
    else
      (* negative families; two's complement big-endian *)
      if n >= IntInf.fromInt ~32 then
        byteStr (IntInf.fromInt 256 + n)             (* negative fixint *)
      else if n >= IntInf.fromInt ~128 then
        byteStr (IntInf.fromInt 0xd0) ^
        beBytes 1 (IntInf.fromInt 256 + n)           (* int8 *)
      else if n >= IntInf.fromInt ~32768 then
        byteStr (IntInf.fromInt 0xd1) ^
        beBytes 2 (pow16 + n)                         (* int16 *)
      else if n >= ~pow31 then
        byteStr (IntInf.fromInt 0xd2) ^
        beBytes 4 (pow32 + n)                         (* int32 *)
      else
        byteStr (IntInf.fromInt 0xd3) ^
        beBytes 8 (pow64 + n)                         (* int64 *)

  fun encodeStr (s : string) : string =
    let val len = String.size s in
      if len <= 31 then
        byteStr (IntInf.fromInt (0xa0 + len)) ^ s              (* fixstr *)
      else if len <= 255 then
        byteStr (IntInf.fromInt 0xd9) ^ beBytes 1 (IntInf.fromInt len) ^ s
      else if len <= 65535 then
        byteStr (IntInf.fromInt 0xda) ^ beBytes 2 (IntInf.fromInt len) ^ s
      else
        byteStr (IntInf.fromInt 0xdb) ^ beBytes 4 (IntInf.fromInt len) ^ s
    end

  fun encodeBin (s : string) : string =
    let val len = String.size s in
      if len <= 255 then
        byteStr (IntInf.fromInt 0xc4) ^ beBytes 1 (IntInf.fromInt len) ^ s
      else if len <= 65535 then
        byteStr (IntInf.fromInt 0xc5) ^ beBytes 2 (IntInf.fromInt len) ^ s
      else
        byteStr (IntInf.fromInt 0xc6) ^ beBytes 4 (IntInf.fromInt len) ^ s
    end

  fun encodeExt (typ : int, data : string) : string =
    let
      val len  = String.size data
      val tb   = byteStr (IntInf.fromInt (typ mod 256))
      fun header code = byteStr (IntInf.fromInt code) ^ tb
    in
      if len = 1 then byteStr (IntInf.fromInt 0xd4) ^ tb ^ data       (* fixext1 *)
      else if len = 2 then byteStr (IntInf.fromInt 0xd5) ^ tb ^ data  (* fixext2 *)
      else if len = 4 then byteStr (IntInf.fromInt 0xd6) ^ tb ^ data  (* fixext4 *)
      else if len = 8 then byteStr (IntInf.fromInt 0xd7) ^ tb ^ data  (* fixext8 *)
      else if len = 16 then byteStr (IntInf.fromInt 0xd8) ^ tb ^ data (* fixext16 *)
      else if len <= 255 then
        byteStr (IntInf.fromInt 0xc7) ^ beBytes 1 (IntInf.fromInt len) ^ tb ^ data
      else if len <= 65535 then
        byteStr (IntInf.fromInt 0xc8) ^ beBytes 2 (IntInf.fromInt len) ^ tb ^ data
      else
        byteStr (IntInf.fromInt 0xc9) ^ beBytes 4 (IntInf.fromInt len) ^ tb ^ data
    end

  fun encode (item : t) : string =
    case item of
      Nil        => byteStr (IntInf.fromInt 0xc0)
    | Bool false => byteStr (IntInf.fromInt 0xc2)
    | Bool true  => byteStr (IntInf.fromInt 0xc3)
    | Int n      => encodeInt n
    | Float r    => byteStr (IntInf.fromInt 0xcb) ^ encodeFloat64 r
    | Str s      => encodeStr s
    | Bin s      => encodeBin s
    | Array elems =>
        let val len = List.length elems
            val hd =
              if len <= 15 then byteStr (IntInf.fromInt (0x90 + len))
              else if len <= 65535 then
                byteStr (IntInf.fromInt 0xdc) ^ beBytes 2 (IntInf.fromInt len)
              else
                byteStr (IntInf.fromInt 0xdd) ^ beBytes 4 (IntInf.fromInt len)
        in hd ^ String.concat (List.map encode elems) end
    | Map pairs =>
        let val len = List.length pairs
            val hd =
              if len <= 15 then byteStr (IntInf.fromInt (0x80 + len))
              else if len <= 65535 then
                byteStr (IntInf.fromInt 0xde) ^ beBytes 2 (IntInf.fromInt len)
              else
                byteStr (IntInf.fromInt 0xdf) ^ beBytes 4 (IntInf.fromInt len)
        in hd ^ String.concat
                  (List.map (fn (k, v) => encode k ^ encode v) pairs) end
    | Ext (typ, data) => encodeExt (typ, data)

  (* ------------------------------------------------------------------ *)
  (* Decoder                                                              *)
  (* ------------------------------------------------------------------ *)

  fun decode (src : string) : t =
    let
      val len = String.size src
      val pos = ref 0

      fun readByte () =
        if !pos >= len then raise Fail "msgpack: unexpected end of input"
        else let val b = Char.ord (String.sub (src, !pos))
             in pos := !pos + 1; b end

      fun readN (n : int) : string =
        if n < 0 orelse !pos + n > len then raise Fail "msgpack: truncated input"
        else let val s = String.substring (src, !pos, n)
             in pos := !pos + n; s end

      (* unsigned big-endian integer from n bytes *)
      fun readUInt (n : int) : IntInf.int =
        let val s = readN n
            fun go (acc, i) =
              if i >= n then acc
              else go (acc * i256 + IntInf.fromInt (Char.ord (String.sub (s, i))), i + 1)
        in go (zero, 0) end

      (* signed big-endian (two's complement) integer from n bytes *)
      fun readSInt (n : int) : IntInf.int =
        let
          val u = readUInt n
          val pw = IntInf.pow (i256, n)
          val half = IntInf.div (pw, IntInf.fromInt 2)
        in if u >= half then u - pw else u end

      fun float64 () : t =
        let
          val s   = readN 8
          fun ob i = Char.ord (String.sub (s, i))
          val b0 = ob 0  val b1 = ob 1  val b2 = ob 2  val b3 = ob 3
          val b4 = ob 4  val b5 = ob 5  val b6 = ob 6  val b7 = ob 7
          val sign = if b0 >= 128 then ~1.0 else 1.0
          val exp  = (b0 mod 128) * 16 + b1 div 16
          val mHi  = (b1 mod 16) * 65536 + b2 * 256 + b3      (* 20 bits *)
          val mLo  = b4 * 16777216 + b5 * 65536 + b6 * 256 + b7  (* 32 bits *)
          val mantR = Real.fromInt mHi * 4294967296.0 + Real.fromInt mLo
        in
          if exp = 0 then
            (* Subnormals and zero. Use an explicit zero fast-path so that
               +0.0 never round-trips to -0.0 (and to avoid relying on
               denormal multiplication, which differs across Poly/ML builds). *)
            if mHi = 0 andalso mLo = 0 then Float 0.0
            else Float (sign * mantR * Math.pow (2.0, ~1074.0))
          else if exp = 2047 then
            if mHi = 0 andalso mLo = 0 then Float (sign * Real.posInf)
            else Float (0.0 / 0.0)
          else
            Float (sign * (1.0 + mantR / 4503599627370496.0) *
                   Math.pow (2.0, Real.fromInt (exp - 1023)))
        end

      fun readItem () : t =
        let val c = readByte () in
          if c <= 0x7f then Int (IntInf.fromInt c)               (* positive fixint *)
          else if c >= 0xe0 then Int (IntInf.fromInt (c - 256))  (* negative fixint *)
          else if c >= 0x80 andalso c <= 0x8f then               (* fixmap *)
            Map (readPairs (c - 0x80))
          else if c >= 0x90 andalso c <= 0x9f then               (* fixarray *)
            Array (readElems (c - 0x90))
          else if c >= 0xa0 andalso c <= 0xbf then               (* fixstr *)
            Str (readN (c - 0xa0))
          else
            case c of
              0xc0 => Nil
            | 0xc2 => Bool false
            | 0xc3 => Bool true
            | 0xc4 => Bin (readN (IntInf.toInt (readUInt 1)))
            | 0xc5 => Bin (readN (IntInf.toInt (readUInt 2)))
            | 0xc6 => Bin (readN (IntInf.toInt (readUInt 4)))
            | 0xc7 => readExt (IntInf.toInt (readUInt 1))
            | 0xc8 => readExt (IntInf.toInt (readUInt 2))
            | 0xc9 => readExt (IntInf.toInt (readUInt 4))
            | 0xca => float32 ()
            | 0xcb => float64 ()
            | 0xcc => Int (readUInt 1)
            | 0xcd => Int (readUInt 2)
            | 0xce => Int (readUInt 4)
            | 0xcf => Int (readUInt 8)
            | 0xd0 => Int (readSInt 1)
            | 0xd1 => Int (readSInt 2)
            | 0xd2 => Int (readSInt 4)
            | 0xd3 => Int (readSInt 8)
            | 0xd4 => readFixext 1
            | 0xd5 => readFixext 2
            | 0xd6 => readFixext 4
            | 0xd7 => readFixext 8
            | 0xd8 => readFixext 16
            | 0xd9 => Str (readN (IntInf.toInt (readUInt 1)))
            | 0xda => Str (readN (IntInf.toInt (readUInt 2)))
            | 0xdb => Str (readN (IntInf.toInt (readUInt 4)))
            | 0xdc => Array (readElems (IntInf.toInt (readUInt 2)))
            | 0xdd => Array (readElems (IntInf.toInt (readUInt 4)))
            | 0xde => Map (readPairs (IntInf.toInt (readUInt 2)))
            | 0xdf => Map (readPairs (IntInf.toInt (readUInt 4)))
            | _    => raise Fail ("msgpack: unknown format byte 0x" ^
                                  Int.toString c)
        end

      and float32 () : t =
        let
          val s   = readN 4
          fun ob i = Char.ord (String.sub (s, i))
          val b0 = ob 0  val b1 = ob 1  val b2 = ob 2  val b3 = ob 3
          val sign = if b0 >= 128 then ~1.0 else 1.0
          val exp  = (b0 mod 128) * 2 + b1 div 128
          val mant = (b1 mod 128) * 65536 + b2 * 256 + b3
        in
          if exp = 0 then
            Float (sign * Real.fromInt mant * Math.pow (2.0, ~149.0))
          else if exp = 255 then
            if mant = 0 then Float (sign * Real.posInf)
            else Float (0.0 / 0.0)
          else
            Float (sign * (1.0 + Real.fromInt mant / 8388608.0) *
                   Math.pow (2.0, Real.fromInt (exp - 127)))
        end

      and readFixext (n : int) : t =
        let val typ = readByte ()
            val data = readN n
        in Ext (typ, data) end

      and readExt (n : int) : t =
        let val typ = readByte ()
            val data = readN n
        in Ext (typ, data) end

      and readElems 0 = []
        | readElems k = let val e = readItem () in e :: readElems (k - 1) end

      and readPairs 0 = []
        | readPairs k =
            let val key = readItem ()
                val v   = readItem ()
            in (key, v) :: readPairs (k - 1) end
    in
      readItem ()
    end

end
