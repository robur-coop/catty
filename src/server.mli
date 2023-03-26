type t

val make : ?tls:bool -> Address.t -> Cri_lwt.send -> t
val uid_of_connection : t -> Uid.t
val uid_of_multiplex : t -> Uid.t
val address : t -> Address.t
val send : t -> ?prefix:Cri.Protocol.prefix -> 'a Cri.Protocol.t -> 'a -> unit
