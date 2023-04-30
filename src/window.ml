open Nottui
open Notty

let src = Logs.Src.create "kit.window"

module Log = (val Logs.src_log src : Logs.LOG)

type t = { w : int; h : int; p : int }

let width_nicknames msgs =
  let f msg acc = max (String.length (Message.nickname msg :> string)) acc in
  Rb.iter ~f msgs 0

let render_message ~width ~width_nicknames msg nicknames =
  let width_message =
    max 1 (width - width_nicknames - 1 - Message.width_time - 1)
  in
  let message = Message.split_at ~len:width_message msg in
  let color =
    try Art.find nicknames (Message.nickname msg) with _exn -> A.white
  in
  let rest =
    List.map @@ fun msg ->
    I.hcat
      [ I.void (Message.width_time + 1 + width_nicknames) 1
      ; I.strf "│"
      ; I.strf "%s" msg
      ]
  in
  I.vcat
    (I.hcat
       [ Message.render_time (Message.time msg)
       ; I.strf " "
       ; I.hsnap ~align:`Right width_nicknames
           (I.strf ~attr:A.(fg color) "%s" (Message.nickname msg :> string))
       ; I.strf "│"
       ; I.strf "%s" (List.hd message)
       ]
    :: rest (List.tl message))

let render { w; h; p } msgs nicknames =
  let idx = ref (Rb.length msgs - 1 - p) in
  let image = ref I.empty in
  let message = ref I.empty in
  let width_nicknames = width_nicknames msgs in
  while
    !idx >= 0
    &&
    (message :=
       render_message ~width_nicknames ~width:w msgs.Rb.%[!idx] nicknames;
     I.height !message + I.height !image <= h)
  do
    (image := I.(!message <-> !image));
    decr idx
  done;
  Ui.atom (I.vsnap ~align:`Bottom h !image)

let handler ~hook state mode msgs key =
  match (key, mode) with
  | (`ASCII 'j', []), `Normal | (`Arrow `Down, []), `Normal ->
      if state.p < Rb.length msgs - 1 then hook { state with p = succ state.p };
      `Handled
  | (`ASCII 'k', []), `Normal | (`Arrow `Up, []), `Normal ->
      if state.p > 0 then hook { state with p = pred state.p };
      `Handled
  | _ -> `Unhandled

let make mode w =
  let ( let* ) x f = Lwd.bind ~f x in
  let ( let+ ) x f = Lwd.map ~f x in
  let ( and+ ) = Lwd.map2 ~f:(fun x y -> (x, y)) in

  let state = Lwd.var { w = 0; h = 0; p = 0 } in
  let hook = Lwd.set state in

  let* document =
    let+ state = Lwd.get state
    and+ mode = Lwd.get mode
    and+ { Windows.nicknames; buffer; _ } = Lwd.get w in

    Ui.keyboard_area
      (handler ~hook state mode buffer)
      (render state buffer nicknames)
  in

  let update_size ~w ~h =
    let state' = Lwd.peek state in
    if state'.w <> w || state'.h <> h then Lwd.set state { state' with w; h }
  in

  let measure_size document =
    Ui.size_sensor update_size (Ui.resize ~sh:1 document)
  in

  Lwd.return (measure_size document)
