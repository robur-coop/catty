type t = { address : Address.t; tls : bool; uid : Uid.t * Uid.t }

let make ?(tls = true) address =
  { address; tls; uid = (Uid.gen (), Uid.gen ()) }

let uid_of_connection { uid; _ } = fst uid
let uid_of_multiplex { uid; _ } = snd uid
let address { address; _ } = address
