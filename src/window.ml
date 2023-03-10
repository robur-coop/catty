open Nottui
open Notty

let src = Logs.Src.create "kit.window"

module Log = (val Logs.src_log src : Logs.LOG)

let split_at ~len:max str =
  let rec go acc off len =
    if len <= max then String.sub str off len :: acc
    else go (String.sub str off max :: acc) (off + max) (len - max) in
  if max <= 0 then invalid_arg "split_at"
  ; go [] 0 (String.length str) |> List.rev

type t = {w: int; h: int; p: int}

let width_nicknames nicknames =
  let res = ref 0 in
  let f (nickname : Art.key) _color () =
    res := max !res (String.length (nickname :> string)) in
  Art.iter ~f () nicknames ; !res

let render_time ptime =
  let (_y, _m, _d), ((hh, mm, ss), _tz) = Ptime.to_date_time ptime in
  I.strf ~attr:A.(fg lightblack) "%02d:%02d:%02d" hh mm ss

let width_ptime = I.width (render_time Ptime.epoch)

let render_message ~width {Message.nickname; message; time} nicknames =
  let width_nicknames = width_nicknames nicknames in
  let width_message = max 1 (width - width_nicknames - 1 - width_ptime - 1) in
  let message = split_at ~len:width_message message in
  let color = Art.find nicknames nickname in
  let rest =
    List.map @@ fun msg ->
    I.hcat
      [
        I.void (width_ptime + 1 + width_nicknames) 1; I.strf "│"; I.strf "%s" msg
      ] in
  I.vcat
    (I.hcat
       [
         render_time time; I.strf " "
       ; I.hsnap ~align:`Right width_nicknames
           (I.strf ~attr:A.(fg color) "%s" (nickname :> string)); I.strf "│"
       ; I.strf "%s" (List.hd message)
       ]
    :: rest (List.tl message))

let render {w; h; p} msgs nicknames =
  let idx = ref (Rb.length msgs - 1 - p) in
  let image = ref I.empty in
  let message = ref I.empty in
  while
    !idx >= 0
    &&
    (message := render_message ~width:w msgs.Rb.%[!idx] nicknames
     ; I.height !message + I.height !image <= h)
  do
    (image := I.(!message <-> !image))
    ; decr idx
  done
  ; Ui.atom (I.vsnap ~align:`Bottom h !image)

let handler ~hook state mode msgs key =
  match key, mode with
  | (`ASCII 'j', []), `Normal | (`Arrow `Down, []), _ ->
    if state.p < Rb.length msgs - 1 then hook {state with p= succ state.p}
    ; `Handled
  | (`ASCII 'k', []), `Normal | (`Arrow `Up, []), _ ->
    if state.p > 0 then hook {state with p= pred state.p}
    ; `Handled
  | _ -> `Unhandled

let make mode w =
  let ( let* ) x f = Lwd.bind ~f x in
  let ( let+ ) x f = Lwd.map ~f x in
  let ( and+ ) = Lwd.map2 ~f:(fun x y -> x, y) in

  let state = Lwd.var {w= 0; h= 0; p= 0} in
  let hook = Lwd.set state in

  let* document =
    let+ state = Lwd.get state
    and+ mode = Lwd.get mode
    and+ nicknames, msgs = Lwd.get w in

    Ui.keyboard_area
      (handler ~hook state mode msgs)
      (render state msgs nicknames) in

  let update_size ~w ~h =
    let state' = Lwd.peek state in
    if state'.w <> w || state'.h <> h then Lwd.set state {state' with w; h}
  in

  let measure_size document =
    Ui.size_sensor update_size (Ui.resize ~sh:1 document) in

  Lwd.return (measure_size document)
