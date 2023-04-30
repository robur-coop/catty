type t =
  | New_message of Uid.t * Message.t
  | New_window of Uid.t * string
  | Set_status of Status.t

let set_status status = Set_status status
let new_message ~uid msg = New_message (uid, msg)
