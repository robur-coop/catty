let src = Logs.Src.create "kit.history"

module Log = (val Logs.src_log src : Logs.LOG)

type t =
  { buffer : (Rb.rdwr, string) Rb.t
  ; mutable position : int
  ; mutable current : Rp.Cursor.cursor option
  }

let make len = { buffer = Rb.make len; position = 0; current = None }

let fill t current =
  t.position <- 0;
  t.current <- None;
  Rb.fit_and_push t.buffer (Rp.to_utf_8_string (Rp.Cursor.to_rope current))

let move_backward t current =
  let len = Rb.length t.buffer in
  if len > 0 && t.position < len then (
    if t.position = 0 then t.current <- Some current;
    let in_the_past = t.buffer.Rb.%[len - t.position - 1] in
    let in_the_past = Rp.of_utf_8_string in_the_past in
    let cursor =
      let max = max 0 (Rp.length in_the_past - 1) in
      min (Rp.Cursor.position current) max
    in
    t.position <- t.position + 1;
    Rp.Cursor.create in_the_past cursor)
  else current

let move_forward t current =
  let len = Rb.length t.buffer in
  if t.position > 0 then (
    t.position <- t.position - 1;
    match (t.position, t.current) with
    | 0, None -> current
    | 0, Some current -> current
    | _ ->
        let in_the_past = t.buffer.Rb.%[len - t.position] in
        let in_the_past = Rp.of_utf_8_string in_the_past in
        let cursor =
          let max = max 0 (Rp.length in_the_past - 1) in
          min (Rp.Cursor.position current) max
        in
        Rp.Cursor.create in_the_past cursor)
  else current
