open Notty
open Nottui

type t =
  [ `Error of string | `Progress of Uchar.t * string | `Done of string | `None ]

let pp ppf = function
  | `Error str -> Fmt.pf ppf "@[<1>(Error %S)@]" str
  | `Progress (uchar, str) ->
      Fmt.pf ppf "@[<1>(Progress %04x %S)@]" (Uchar.to_int uchar) str
  | `Done str -> Fmt.pf ppf "@[<1>(Done %S)@]" str
  | `None -> Fmt.pf ppf "None"

let render = function
  | `None -> I.uchars A.empty [| Uchar.of_int 0x2205 |] |> Ui.atom
  | `Error text ->
      I.hcat
        [ I.uchars A.empty [| Uchar.of_int 0x2203 |]
        ; I.char A.empty ' ' 1 1
        ; I.strf ~attr:A.(bg red ++ fg white) "%s" text
        ]
      |> Ui.atom
  | `Progress (uchr, text) ->
      I.hcat
        [ I.uchars A.empty [| uchr |]
        ; I.char A.empty ' ' 1 1
        ; I.strf ~attr:A.empty "%s" text
        ]
      |> Ui.atom
  | `Done text ->
      I.hcat
        [ I.uchars A.empty [| Uchar.of_int 0x2261 |]
        ; I.char A.empty ' ' 1 1
        ; I.strf ~attr:A.empty "%s" text
        ]
      |> Ui.atom

let loading =
  let tbl =
    [| Uchar.of_int 0x280B (* "⠋" *)
     ; Uchar.of_int 0x2819 (* "⠙" *)
     ; Uchar.of_int 0x2839 (* "⠹" *)
     ; Uchar.of_int 0x2838 (* "⠸" *)
     ; Uchar.of_int 0x283C (* "⠼" *)
     ; Uchar.of_int 0x2834 (* "⠴" *)
     ; Uchar.of_int 0x2826 (* "⠦" *)
     ; Uchar.of_int 0x2827 (* "⠧" *)
     ; Uchar.of_int 0x2807 (* "⠇" *)
     ; Uchar.of_int 0x280F (* "⠏" *)
    |]
  in
  fun ~sleep ~stop ~text action ->
    let open Lwt.Infix in
    let open Lwt.Syntax in
    let stop () =
      let+ () = stop in
      `Stop
    in
    let continue () =
      let+ () = sleep 0.1 in
      `Continue
    in
    let rec go idx =
      if idx = Array.length tbl then go 0
      else (
        action (`Progress (tbl.(idx), text));
        Lwt.choose [ continue (); stop () ] >>= function
        | `Continue -> go (succ idx)
        | `Stop -> Lwt.return_unit)
    in
    go 0

let errorf fmt = Fmt.kstr (fun msg -> `Error msg) fmt
