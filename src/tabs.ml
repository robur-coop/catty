open Notty
open Nottui

type t = { tabs : [ `Console | `Window of string ] list; active : int }

let empty = { tabs = [ `Console ]; active = 0 }

let next t =
  if t.active + 1 >= List.length t.tabs then t
  else { t with active = succ t.active }

let prev t = if t.active <= 0 then t else { t with active = pred t.active }

let render t =
  let tabs =
    List.mapi @@ fun idx -> function
    | `Console ->
        if idx = t.active then
          I.
            [ strf ~attr:A.(fg red) "["
            ; strf "console"
            ; strf ~attr:A.(fg red) "]"
            ]
          |> I.hcat
        else I.strf " console "
    | `Window title ->
        if idx = t.active then
          I.
            [ strf ~attr:A.(fg yellow) "["
            ; strf "%s" title
            ; strf ~attr:A.(fg yellow) "]"
            ]
          |> I.hcat
        else I.strf " %s " title
  in
  tabs t.tabs |> I.vcat |> Ui.atom

let handler ~hook:_ _t = function _ -> `Unhandled

let make tabs =
  let ( let* ) x f = Lwd.bind ~f x in
  let ( let+ ) x f = Lwd.map ~f x in

  let hook = Lwd.set tabs in

  let* document =
    let+ tabs = Lwd.get tabs in
    Ui.keyboard_area (handler ~hook tabs) (render tabs)
  in
  Lwd.return document
