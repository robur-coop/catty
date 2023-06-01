type t =
  | New_message of Uid.t * Message.t
  | New_window of Uid.t * string
  | Delete_window of Uid.t
  | Set_status of Status.t

val set_status : Status.t -> t
val new_message : uid:Uid.t -> Message.t -> t
val new_window : uid:Uid.t -> string -> t
val delete_window : uid:Uid.t -> t
val pp : t Fmt.t
