type t = { nickname : Art.key; message : string list; time : Ptime.t }

let pp ppf { nickname; message; time } =
  Fmt.pf ppf "(%a)%s:%a" (Ptime.pp_rfc3339 ()) time
    (nickname :> string)
    Fmt.(Dump.list string)
    message

let nickname { nickname; _ } = nickname
let time { time; _ } = time

let split_at ~len:max str =
  let rec go acc off len =
    if len <= max then String.sub str off len :: acc
    else go (String.sub str off max :: acc) (off + max) (len - max)
  in
  if max <= 0 then invalid_arg "split_at";
  go [] 0 (String.length str) |> List.rev

let split_at ~len { message; _ } =
  List.map (split_at ~len) message |> List.concat

let render_time ptime =
  let open Notty in
  let (_y, _m, _d), ((hh, mm, ss), _tz) = Ptime.to_date_time ptime in
  I.strf ~attr:A.(fg lightblack) "%02d:%02d:%02d" hh mm ss

let width_time = Notty.I.width (render_time Ptime.epoch)
let make ~nickname ~time message = { nickname; time; message }

let msgf ~now ?prefix ?server fmt =
  let nickname =
    match (prefix, server) with
    | None, Some server ->
        Server.address server |> Fmt.to_to_string Address.pp |> Art.key
    | Some prefix, _ ->
        Fmt.to_to_string Cri.Protocol.pp_prefix prefix |> Art.key
    | None, None -> Art.key "kitty"
  in
  Fmt.kstr
    (fun message ->
      let message = String.split_on_char '\n' message in
      { nickname; time = now (); message })
    fmt
