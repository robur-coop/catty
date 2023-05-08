let () = Printexc.record_backtrace true
let () = Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)

module TCP = struct
  open Lwt.Infix

  type flow = Lwt_unix.file_descr
  type error = [ `Refused | `Timeout | `Error of Unix.error * string * string ]

  type write_error =
    [ `Refused | `Timeout | `Closed | `Error of Unix.error * string * string ]

  let pp_error ppf = function
    | `Error (err, f, v) ->
        Fmt.pf ppf "%s(%s) : %s" f v (Unix.error_message err)
    | `Refused -> Fmt.pf ppf "Connection refused"
    | `Timeout -> Fmt.pf ppf "Connection timeout"

  let pp_write_error ppf = function
    | #error as err -> pp_error ppf err
    | `Closed -> Fmt.pf ppf "Connection closed by peer"

  let pp_sockaddr ppf = function
    | Unix.ADDR_INET (inet_addr, port) ->
        Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
    | Unix.ADDR_UNIX v -> Fmt.pf ppf "<%s>" v

  let read fd =
    let tmp = Bytes.create 0x1000 in
    let process () =
      Lwt_unix.read fd tmp 0 (Bytes.length tmp) >>= function
      | 0 -> Lwt.return_ok `Eof
      | len -> Lwt.return_ok (`Data (Cstruct.of_bytes ~off:0 ~len tmp))
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) ->
        Logs.err (fun m ->
            m "Got an error: %s(%s): %s" f v (Unix.error_message e));
        Lwt.return_error (`Error (e, f, v))
    | exn -> Lwt.fail exn

  let write fd ({ Cstruct.len; _ } as cs) =
    let rec process buf off max =
      Lwt_unix.write fd buf off max >>= fun len ->
      if max - len = 0 then Lwt.return_ok ()
      else process buf (off + len) (max - len)
    in
    let buf = Cstruct.to_bytes cs in
    Lwt.catch (fun () -> process buf 0 len) @@ function
    | Unix.Unix_error (e, f, v) ->
        Logs.err (fun m ->
            m "Got an error: %s(%s): %s" f v (Unix.error_message e));
        Lwt.return_error (`Error (e, f, v))
    | exn -> Lwt.fail exn

  let rec writev fd = function
    | [] -> Lwt.return_ok ()
    | x :: r -> Lwt_result.bind (write fd x) (fun () -> writev fd r)

  let close fd = Lwt_unix.close fd

  type endpoint = Lwt_unix.sockaddr

  let connect sockaddr =
    let process () =
      let fam = Unix.domain_of_sockaddr sockaddr in
      let socket = Lwt_unix.socket fam Unix.SOCK_STREAM 0 in
      Logs.debug (fun m ->
          m "Start a TCP/IP connection to: %a" pp_sockaddr sockaddr);
      Lwt_unix.connect socket sockaddr >>= fun () ->
      Logs.debug (fun m -> m "Connected! (TCP/IP)");
      Lwt.return_ok socket
    in
    Lwt.catch process @@ function
    | Unix.Unix_error (e, f, v) ->
        Logs.err (fun m ->
            m "Impossible to connect to %a: %s(%s): %s" pp_sockaddr sockaddr f v
              (Unix.error_message e));
        Lwt.return_error (`Error (e, f, v))
    | exn ->
        Logs.err (fun m -> m "Got an exception: %S" (Printexc.to_string exn));
        raise exn
end

module TLS = struct
  include Tls_mirage.Make (TCP)

  type endpoint =
    Tls.Config.client * [ `host ] Domain_name.t option * Lwt_unix.sockaddr

  let connect (tls, host, sockaddr) =
    let open Lwt.Infix in
    Lwt_result.bind
      (TCP.connect sockaddr >|= Result.map_error (fun err -> `Read err))
      (fun flow ->
        Logs.debug (fun m ->
            m "Start a TLS connection to: %a" TCP.pp_sockaddr sockaddr);
        client_of_flow tls ?host flow >>= fun res ->
        Logs.debug (fun m ->
            m "Connection to %a completed!" TCP.pp_sockaddr sockaddr);
        Lwt.return res)
    >>= function
    | Ok _ as value -> Lwt.return value
    | Error err ->
        Logs.err (fun m ->
            m "Impossible to connect to %a (TLS): %a" TCP.pp_sockaddr sockaddr
              pp_write_error err);
        Lwt.return_error err
end

let tcp_edn, tcp_protocol = Mimic.register ~name:"tcp/ip" (module TCP)
let tls_edn, tls_protocol = Mimic.register ~priority:10 ~name:"tls" (module TLS)

let ctx =
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok v -> v
    | Error (`Msg err) -> failwith err
  in
  let to_sockaddr = function
    | `Domain (host, port) ->
        Logs.debug (fun m -> m "Resolving %a" Domain_name.pp host);
        let { Unix.h_addr_list; _ } =
          Unix.gethostbyname (Domain_name.to_string host)
        in
        Logs.debug (fun m ->
            m "%a resolved: %s" Domain_name.pp host
              (Unix.string_of_inet_addr h_addr_list.(0)));
        (Some host, Unix.ADDR_INET (h_addr_list.(0), port))
    | `Inet (ipaddr, port) ->
        (None, Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port))
  in

  let k0 dst with_tls =
    try
      let host, sockaddr = to_sockaddr dst in
      match with_tls with
      | true ->
          Lwt.return_some (Tls.Config.client ~authenticator (), host, sockaddr)
      | false -> Lwt.return_none
    with _ -> Lwt.return_none
  in
  let k1 dst with_tls =
    try
      let _host, sockaddr = to_sockaddr dst in
      match with_tls with
      | true -> Lwt.return_none
      | false -> Lwt.return_some sockaddr
    with _ -> Lwt.return_none
  in
  Mimic.empty
  |> Mimic.fold ~k:k0 tls_edn
       Mimic.Fun.[ req Kit.Engine.Connect.dst; req Kit.Engine.Connect.tls ]
  |> Mimic.fold ~k:k1 tcp_edn
       Mimic.Fun.[ req Kit.Engine.Connect.dst; req Kit.Engine.Connect.tls ]

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () =
  let oc = open_out "log.txt" in
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ();
  Logs.set_reporter (reporter (Format.formatter_of_out_channel oc));
  Logs.set_level ~all:true (Some Logs.Debug);
  Stdlib.at_exit (fun () -> close_out oc)

let now () = Ptime_clock.now ()

let fiber host nicknames realname =
  let ui, cursor, quit, engine, action =
    let ( let* ) x f = Lwd.bind ~f x in
    let cursor = Lwd.var (0, 0) in
    let user = Kit.User.make ~realname nicknames in
    let (quit, command, message, action), engine =
      Kit.Engine.make ~ctx ~now ~sleep:Lwt_unix.sleep ~host user
    in
    let mode = Lwd.var `Normal in
    let status = Lwd.var `None in
    let windows = Kit.Windows.make ~now host in

    let rec unroll_action () =
      let open Lwt.Infix in
      Lwt_stream.get action >>= function
      | None -> Lwt.return_unit
      | Some action -> (
          Logs.debug (fun m -> m "Got a new action: %a" Kit.Action.pp action);
          match action with
          | Kit.Action.Set_status v ->
              Lwd.set status v;
              unroll_action ()
          | Kit.Action.New_window (uid, name) ->
              Kit.Windows.new_window windows ~uid ~name >>= unroll_action
          | Kit.Action.Delete_window uid ->
              Kit.Windows.delete_window windows ~uid >>= unroll_action
          | Kit.Action.New_message (uid, msg) ->
              Kit.Windows.push_on windows ~uid msg >>= unroll_action)
    in

    let ui =
      let* prompt =
        Kit.Prompt.make ~command ~message cursor status mode
          (Kit.Windows.var windows)
      in
      let* window = Kit.Windows.Ui.make mode windows in
      Lwd.return (Nottui.Ui.vcat [ window; prompt ])
    in
    (ui, cursor, quit, engine, unroll_action)
  in
  Lwt.join
    [ Nottui_lwt.run ~cursor ~quit ui; Kit.Engine.process engine; action () ]

open Cmdliner

let hostname =
  let parser str = Domain_name.of_string str in
  let pp = Domain_name.pp in
  Arg.conv (parser, pp) ~docv:"<hostname>"

let nickname =
  let parser str = Cri.Nickname.of_string str in
  let pp = Cri.Nickname.pp in
  Arg.conv (parser, pp) ~docv:"<nickname>"

let hostname =
  let doc = "The hostname of the system." in
  let default = Domain_name.of_string_exn (Unix.gethostname ()) in
  Arg.(value & opt hostname default & info [ "h"; "hostname" ] ~doc)

let nicknames =
  let doc = "The nickname of the user." in
  let default = [ Cri.Nickname.of_string_exn (Unix.getlogin ()) ] in
  Arg.(non_empty & opt_all nickname default & info [ "n"; "nickname" ] ~doc)

let realname =
  let doc = "The realname of the user." in
  Arg.(required & opt (some string) None & info [ "realname" ] ~doc)

let run hostname nickname realname =
  Lwt_main.run (fiber hostname nickname realname)

let kitty_info = Cmd.info ~doc:"An IRC client in OCaml" "kitty"
let term = Term.(const run $ hostname $ nicknames $ realname)
let () = Cmd.(eval (v kitty_info term)) |> exit
