open Notty
open Nottui

type t =
  [ `Error of string | `Progress of Uchar.t * string | `Done of string | `None ]

let render = function
  | `None -> I.uchars A.empty [|Uchar.of_int 0x2205|] |> Ui.atom
  | `Error text ->
    I.hcat
      [
        I.uchars A.empty [|Uchar.of_int 0x2203|]; I.char A.empty ' ' 1 1
      ; I.strf ~attr:A.(bg red ++ fg white) "%s" text
      ]
    |> Ui.atom
  | `Progress (uchr, text) ->
    I.hcat
      [
        I.uchars A.empty [|uchr|]; I.char A.empty ' ' 1 1
      ; I.strf ~attr:A.empty "%s" text
      ]
    |> Ui.atom
  | `Done text ->
    I.hcat
      [
        I.uchars A.empty [|Uchar.of_int 0x2261|]; I.char A.empty ' ' 1 1
      ; I.strf ~attr:A.empty "%s" text
      ]
    |> Ui.atom
