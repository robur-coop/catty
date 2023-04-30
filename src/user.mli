type t

val make : realname:string -> ?username:string -> Cri.Nickname.t list -> t
val nicknames : t -> Cri.Nickname.t list
val realname : t -> string
val username : t -> string

val user :
     ?with_nickname:Cri.Nickname.t
  -> ?mode:Cri.User_mode.t list
  -> t
  -> Cri.Protocol.user
