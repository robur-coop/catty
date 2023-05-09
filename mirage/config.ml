open Mirage

let port =
  let doc = Key.Arg.info ~doc:"Port of the SSH service." [ "p"; "port" ] in
  Key.(create "port" Arg.(opt int 22 doc))

let users =
  let doc =
    Key.Arg.info ~doc:"An user with its authentication mechanism."
      [ "u"; "user" ]
  in
  Key.(create "users" Arg.(opt_all string doc))

let private_key =
  let doc =
    Key.Arg.info ~doc:"The private key of the SSH service." [ "private-key" ]
  in
  Key.(create "private_key" Arg.(required string doc))

let realname =
  let doc = Key.Arg.info ~doc:"The realname of the user." [ "realname" ] in
  Key.(create "realname" Arg.(required string doc))

let nicknames =
  let doc =
    Key.Arg.info ~doc:"Possible nicknames of the user." [ "nickname" ]
  in
  Key.(create "nicknames" Arg.(opt_all string doc))

let host =
  let doc = Key.Arg.info ~doc:"The host of the unikernel." [ "host" ] in
  Key.(create "host" Arg.(required string doc))

let catty =
  let awa_pin = "git+https://github.com/mirage/awa-ssh.git" in
  let lwd_pin =
    "git+https://github.com/dinosaure/lwd.git#9e78758d5987597bac65fe73bd30ff80741cfe83"
  in
  foreign "Unikernel.Make"
    ~packages:
      [
        package ~pin:awa_pin "awa-mirage";
        package ~pin:awa_pin "awa";
        package "hxd" ~sublibs:[ "core"; "string" ];
        package "catty";
        package "rresult";
        package "bigstringaf";
        package "notty";
        package ~pin:lwd_pin "nottui";
        package ~pin:lwd_pin "lwd";
      ]
    ~keys:
      [
        Key.v port;
        Key.v private_key;
        Key.v users;
        Key.v realname;
        Key.v realname;
        Key.v nicknames;
        Key.v host;
      ]
    (random @-> time @-> mclock @-> stackv4v6 @-> job)

let stackv4v6 = generic_stackv4v6 default_network

let () =
  register "catty"
    [
      catty $ default_random $ default_time $ default_monotonic_clock $ stackv4v6;
    ]
