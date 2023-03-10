include Rope.Make_array (struct
  include Uchar

  let uchar_to_utf_8 =
    let buf = Buffer.create 16 in
    fun uchar ->
      Uutf.Buffer.add_utf_8 buf uchar
      ; let res = Buffer.contents buf in
        Buffer.clear buf ; res

  let print =
    Fmt.if_utf_8
      Fmt.(using uchar_to_utf_8 string)
      Fmt.(using Uchar.to_int (any "U+04X"))
end)
