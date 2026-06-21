(* test.sml — MessagePack (spec v2.0) test suite with exact byte vectors *)
structure Tests =
struct

  (* Build a one-byte string from an int 0-255 *)
  fun b (n : int) : string = String.str (Char.chr n)
  fun bs (ns : int list) : string = String.concat (List.map b ns)

  (* Convert a string to a hex dump for error messages *)
  fun hexDump (s : string) : string =
    String.concat
      (List.map
        (fn c =>
          let val n = Char.ord c
              val hi = n div 16
              val lo = n mod 16
              fun hex d = if d < 10 then Char.chr (d + 48)
                          else Char.chr (d + 87)
          in String.implode [hex hi, hex lo, #" "] end)
        (String.explode s))

  fun fromBytes (ns : int list) : string = bs ns
  fun bytes (s : string) : int list = List.map Char.ord (String.explode s)

  fun checkEncode (name : string) (item : Msgpack.t) (expected : string) =
    let val actual = Msgpack.encode item
    in
      if actual = expected then
        Harness.check name true
      else
        ( print ("  FAIL - " ^ name ^ ": expected " ^ hexDump expected ^
                 " got " ^ hexDump actual ^ "\n")
        ; Harness.check name false )
    end

  fun roundtrip name item =
    let val enc = Msgpack.encode item
        val dec = Msgpack.decode enc
    in
      if Msgpack.encode dec = enc then
        Harness.check name true
      else
        ( print ("  FAIL - " ^ name ^ ": roundtrip mismatch " ^
                 hexDump enc ^ "vs " ^ hexDump (Msgpack.encode dec) ^ "\n")
        ; Harness.check name false )
    end

  fun run () =
    let
      val () = Harness.reset ()

      (* ---------------------------------------------------------------- *)
      (* Section 1: Nil and Bool                                          *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Nil and Bool"

      val () = checkEncode "Nil"        Msgpack.Nil          (b 0xc0)
      val () = checkEncode "Bool true"  (Msgpack.Bool true)  (b 0xc3)
      val () = checkEncode "Bool false" (Msgpack.Bool false) (b 0xc2)

      (* ---------------------------------------------------------------- *)
      (* Section 2: Integers                                              *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Integers"

      fun ii n = Msgpack.Int (IntInf.fromInt n)

      val () = checkEncode "Int 0"      (ii 0)    (b 0x00)
      val () = checkEncode "Int 127"    (ii 127)  (b 0x7f)
      val () = checkEncode "Int ~1"     (ii ~1)   (b 0xff)
      val () = checkEncode "Int ~32"    (ii ~32)  (b 0xe0)
      val () = checkEncode "Int 128"    (ii 128)  (bs [0xcc, 0x80])
      val () = checkEncode "Int 256"    (ii 256)  (bs [0xcd, 0x01, 0x00])
      val () = checkEncode "Int ~128"   (ii ~128) (bs [0xd0, 0x80])
      val () = checkEncode "Int ~32768" (ii ~32768) (bs [0xd1, 0x80, 0x00])

      (* ---------------------------------------------------------------- *)
      (* Section 3: Str and Bin                                           *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Str and Bin"

      val () = checkEncode "Str empty" (Msgpack.Str "") (b 0xa0)
      val () = checkEncode "Str hello"
                 (Msgpack.Str "hello")
                 (bs [0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f])

      (* 32-char string -> str8 (0xd9 0x20 ...) *)
      val str32 = String.implode (List.tabulate (32, fn _ => #"x"))
      val () = checkEncode "Str 32 chars -> str8"
                 (Msgpack.Str str32)
                 (bs [0xd9, 0x20] ^ str32)

      val () = checkEncode "Bin de ad"
                 (Msgpack.Bin (bs [0xde, 0xad]))
                 (bs [0xc4, 0x02, 0xde, 0xad])

      (* ---------------------------------------------------------------- *)
      (* Section 4: Array and Map                                         *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Array and Map"

      val () = checkEncode "Array empty" (Msgpack.Array []) (b 0x90)
      val () = checkEncode "Array [1,2,3]"
                 (Msgpack.Array [ii 1, ii 2, ii 3])
                 (bs [0x93, 0x01, 0x02, 0x03])

      val () = checkEncode "Map empty" (Msgpack.Map []) (b 0x80)
      val () = checkEncode "Map [(a,1)]"
                 (Msgpack.Map [(Msgpack.Str "a", ii 1)])
                 (bs [0x81, 0xa1, 0x61, 0x01])

      (* ---------------------------------------------------------------- *)
      (* Section 5: Float and Ext                                         *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Float and Ext"

      (* Float always encodes as float64 (0xcb) + 8 bytes *)
      val () = Harness.checkInt "Float 3.14 length"
                 (9, String.size (Msgpack.encode (Msgpack.Float 3.14)))
      val () = Harness.checkInt "Float 3.14 tag"
                 (0xcb, Char.ord (String.sub (Msgpack.encode (Msgpack.Float 3.14), 0)))

      val () = checkEncode "Ext(1, 0x02)"
                 (Msgpack.Ext (1, b 0x02))
                 (bs [0xd4, 0x01, 0x02])  (* fixext1 *)

      (* ---------------------------------------------------------------- *)
      (* Section 6: Roundtrip and errors                                  *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Roundtrip and errors"

      val () = roundtrip "RT Nil"        Msgpack.Nil
      val () = roundtrip "RT Bool true"  (Msgpack.Bool true)
      val () = roundtrip "RT Bool false" (Msgpack.Bool false)
      val () = roundtrip "RT Int 0"      (ii 0)
      val () = roundtrip "RT Int 127"    (ii 127)
      val () = roundtrip "RT Int 128"    (ii 128)
      val () = roundtrip "RT Int 256"    (ii 256)
      val () = roundtrip "RT Int ~1"     (ii ~1)
      val () = roundtrip "RT Int ~32"    (ii ~32)
      val () = roundtrip "RT Int ~128"   (ii ~128)
      val () = roundtrip "RT Int ~32768" (ii ~32768)
      val () = roundtrip "RT Int 70000"  (ii 70000)
      val () = roundtrip "RT Int big"    (Msgpack.Int (IntInf.* (IntInf.fromInt 65536, IntInf.fromInt 65536)))
      val () = roundtrip "RT Str empty"  (Msgpack.Str "")
      val () = roundtrip "RT Str hello"  (Msgpack.Str "hello")
      val () = roundtrip "RT Str 32"     (Msgpack.Str str32)
      val () = roundtrip "RT Bin"        (Msgpack.Bin (bs [0xde, 0xad]))
      val () = roundtrip "RT Array"      (Msgpack.Array [ii 1, ii 2, ii 3])
      val () = roundtrip "RT Map"        (Msgpack.Map [(Msgpack.Str "a", ii 1)])
      val () = roundtrip "RT Ext"        (Msgpack.Ext (1, b 0x02))

      (* Float roundtrips: compare decoded real value *)
      fun rtFloat name r =
        let val dec = Msgpack.decode (Msgpack.encode (Msgpack.Float r))
        in case dec of
             Msgpack.Float r' => Harness.check name (Real.== (r, r'))
           | _ => Harness.check name false
        end
      val () = rtFloat "RT Float 3.14" 3.14
      val () = rtFloat "RT Float 0.0"  0.0
      val () = rtFloat "RT Float ~1.0" ~1.0

      (* Error: truncated input raises Fail *)
      val () = Harness.checkRaises "decode truncated str"
                 (fn () => Msgpack.decode (bs [0xa5, 0x68]))
      val () = Harness.checkRaises "decode truncated int"
                 (fn () => Msgpack.decode (bs [0xcd, 0x01]))
      val () = Harness.checkRaises "decode empty"
                 (fn () => Msgpack.decode "")

    in
      Harness.run ()
    end

end
