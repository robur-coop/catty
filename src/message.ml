type t = {nickname: Art.key; message: string; time: Ptime.t}

let pp ppf {time; nickname; message} =
  Fmt.pf ppf "%a %s:%s" (Ptime.pp_rfc3339 ()) time (nickname :> string) message

let make ~nickname ~time message = {nickname; time; message}
