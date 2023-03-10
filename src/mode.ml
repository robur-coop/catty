type t = [ `Normal | `Insertion | `Command of Rp.Cursor.cursor | `Visual ]

open Notty

let pretty = function
  | `Insertion -> I.uchars A.(fg yellow) [|Uchar.of_int 0x03b9|]
  | `Normal -> I.uchars A.(fg blue) [|Uchar.of_int 0x03b7|]
  | `Command _ -> I.uchars A.(fg green) [|Uchar.of_int 0x03c0|]
  | `Visual -> I.uchars A.(fg cyan) [|Uchar.of_int 0x03bd|]
