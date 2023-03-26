type t =
  { address : Address.t
  ; tls : bool
  ; uid : Uid.t * Uid.t
  ; sender : Cri_lwt.send
  }

let make ?(tls = true) address sender =
  { address; tls; uid = (Uid.gen (), Uid.gen ()); sender }

let uid_of_connection { uid; _ } = fst uid
let uid_of_multiplex { uid; _ } = snd uid
let address { address; _ } = address
let send t ?prefix w v = t.sender.send ?prefix w v
