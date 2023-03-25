type t

val make : ?tls:bool -> Address.t -> t
val uid_of_connection : t -> Uid.t
val uid_of_multiplex : t -> Uid.t
val address : t -> Address.t
