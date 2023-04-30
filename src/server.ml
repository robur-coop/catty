type t = { address : Address.t; tls : bool; uid : Uid.t; sender : Cri_lwt.send }

let pp ppf { address; tls; _ } =
  match tls with
  | true -> Fmt.pf ppf "tls://%a" Address.pp address
  | false -> Fmt.pf ppf "tcp://%a" Address.pp address

let make ?(tls = true) address sender =
  { address; tls; uid = Uid.gen (); sender }

let uid { uid; _ } = uid
let address { address; _ } = address
let send t ?prefix w v = t.sender.send ?prefix w v
