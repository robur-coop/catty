let src = Logs.Src.create "kit.message"

module Log = (val Logs.src_log src : Logs.LOG)

type t = { nickname : Art.key; message : string list; time : Ptime.t }

let pp ppf { nickname; message; time } =
  Fmt.pf ppf "(%a)%s:%a" (Ptime.pp_rfc3339 ()) time
    (nickname :> string)
    Fmt.(Dump.list string)
    message

let nickname { nickname; _ } = nickname
let time { time; _ } = time

let split_at ~len:max utf_8_str =
  if max <= 0 then invalid_arg "split_at";
  let folder acc _pos = function
    | `Malformed _ -> Uutf.u_rep :: acc
    | `Uchar uchr -> uchr :: acc
  in
  let rec go acc len lst =
    match (acc, lst) with
    | [], [] -> [ [] ]
    | hd :: tl, x :: r ->
        if len < max then go ((x :: hd) :: tl) (succ len) r
        else go ([ x ] :: hd :: tl) 0 r
    | [], x :: r ->
        assert (len = 0);
        go [ [ x ] ] 1 r
    | hd :: tl, [] -> List.rev (hd :: tl)
  in
  let lst = Uutf.String.fold_utf_8 folder [] utf_8_str in
  let lst = List.rev lst in
  let lst = go [] 0 lst in
  let lst = List.rev_map List.rev lst in
  let lst = List.map Array.of_list lst in
  List.rev_map Notty.(I.uchars A.empty) lst

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
    match ((prefix : Cri.Protocol.prefix option), server) with
    | None, Some server ->
        Server.address server |> Fmt.to_to_string Address.pp |> Art.key
    | Some prefix, _ ->
        let prefix =
          match prefix with
          | Cri.Protocol.Server host ->
              Fmt.to_to_string Cri.Protocol.pp_host host
          | Cri.Protocol.User { name; _ } -> Cri.Nickname.to_string name
        in
        Art.key prefix
    | None, None -> Art.key "kitty"
  in
  Fmt.kstr
    (fun message ->
      let message = String.split_on_char '\n' message in
      { nickname; time = now (); message })
    fmt
