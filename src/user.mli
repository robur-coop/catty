type t

val make : realname:string -> ?username:string -> Cri.Nickname.t -> t
val nickname : t -> Cri.Nickname.t
val realname : t -> string
val username : t -> string
val user : ?mode:Cri.User_mode.t list -> t -> Cri.Protocol.user
