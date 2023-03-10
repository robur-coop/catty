open Mirage

let port =
  let doc = Key.Arg.info ~doc:"Port of the SSH service." ["p"; "port"] in
  Key.(create "port" Arg.(opt int 22 doc))

let users =
  let doc =
    Key.Arg.info ~doc:"An user with its authentication mechanism." ["u"; "user"]
  in
  Key.(create "users" Arg.(opt_all string doc))

let private_key =
  let doc =
    Key.Arg.info ~doc:"The private key of the SSH service." ["private-key"]
  in
  Key.(create "private_key" Arg.(required string doc))

let kit =
  let awa_pin =
    "git+https://github.com/dinosaure/awa-ssh.git#handle-pty-and-shell" in
  foreign "Unikernel.Make"
    ~packages:
      [
        package ~pin:awa_pin "awa-mirage"; package ~pin:awa_pin "awa"
      ; package "hxd" ~sublibs:["core"; "string"]; package "kit"
      ; package "rresult"; package "bigstringaf"; package "notty"
      ; package "nottui"; package "lwd"
      ]
    ~keys:[Key.v port; Key.v private_key; Key.v users]
    (random @-> time @-> mclock @-> stackv4v6 @-> job)

let stackv4v6 = generic_stackv4v6 default_network

let () =
  register "kit"
    [kit $ default_random $ default_time $ default_monotonic_clock $ stackv4v6]
