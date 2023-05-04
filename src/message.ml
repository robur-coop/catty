type t = { nickname : Art.key; message : string list; time : Ptime.t }

let nickname { nickname; _ } = nickname
let time { time; _ } = time

let split_at ~len:max str =
  let buf = Buffer.create max in
  let add_utf_8_character buf = function
    | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep
    | `Uchar uchr -> Uutf.Buffer.add_utf_8 buf uchr
  in
  let folder (acc, len) _off uchr =
    if len >= max then (
      let str = Buffer.contents buf in
      Buffer.clear buf;
      add_utf_8_character buf uchr;
      (str :: acc, 0))
    else (
      add_utf_8_character buf uchr;
      (acc, succ len))
  in
  if max <= 0 then invalid_arg "split_at";
  Uutf.String.fold_utf_8 folder ([], 0) str |> fun (acc, _) -> List.rev acc

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
