let src = Logs.Src.create "kit.tabs"

module Log = (val Logs.src_log src : Logs.LOG)
open Notty
open Nottui

type t =
  { tabs : [ `Console | `Window of string ] list; active : int; max : int }

let pp ppf { tabs; active; max } =
  let pp_elt ppf = function
    | `Console -> Fmt.string ppf "console"
    | `Window str -> Fmt.string ppf str
  in
  Fmt.pf ppf "@[<hov>{ tabs= @[<hov>%a@];@ active= %d;@ max= %d; }@]"
    Fmt.(Dump.list pp_elt)
    tabs active max

let empty = { tabs = [ `Console ]; active = 0; max = 1 }

let v ?(active = 0) = function
  | [] -> invalid_arg "The list of tabs can not be empty"
  | tabs ->
      if active >= List.length tabs then
        invalid_arg "The active window does not exist";
      { tabs; active; max = List.length tabs }

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
  tabs t.tabs |> I.hcat |> Ui.atom

let sup = 1
and inf = -1

let handler ~hook tabs = function
  | `ASCII 'N', [ `Ctrl ] ->
      hook sup tabs;
      `Handled
  | `ASCII 'P', [ `Ctrl ] ->
      hook inf tabs;
      `Handled
  | _ -> `Unhandled

let make ~move tabs =
  let ( let* ) x f = Lwd.bind ~f x in
  let ( let+ ) x f = Lwd.map ~f x in

  let* document =
    let+ tabs = Lwd.get tabs in
    Ui.keyboard_area (handler ~hook:move tabs) (render tabs)
  in
  Lwd.return document
