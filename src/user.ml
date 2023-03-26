type t = { nickname : Cri.Nickname.t; username : string; realname : string }

let make ~realname ?username nickname =
  let username =
    match username with
    | None -> Cri.Nickname.to_string nickname
    | Some username -> username
  in
  { nickname; username; realname }

let nickname { nickname; _ } = nickname
let realname { realname; _ } = realname
let username { username; _ } = username

let user ?(mode = []) { username; realname; _ } =
  let mode = Cri.User_mode.to_int mode in
  { Cri.Protocol.username; realname; mode }
