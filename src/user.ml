type t =
  { nicknames : Cri.Nickname.t list; username : string; realname : string }

let make ~realname ?username = function
  | [] -> invalid_arg "An user must have, at least, one nickname"
  | nickname :: _ as nicknames ->
      let username =
        match username with
        | None -> Cri.Nickname.to_string nickname
        | Some username -> username
      in
      { nicknames; username; realname }

let nicknames { nicknames; _ } = nicknames
let realname { realname; _ } = realname
let username { username; _ } = username

let user ?with_nickname ?(mode = []) { username; realname; _ } =
  let username =
    match with_nickname with
    | Some nickname -> Cri.Nickname.to_string nickname
    | None -> username
  in
  let mode = Cri.User_mode.to_int mode in
  { Cri.Protocol.username; realname; mode }
