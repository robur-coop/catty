open Lwt.Infix
open Lwt.Syntax

let src = Logs.Src.create "kit.engine"

module Log = (val Logs.src_log src : Logs.LOG)

type vars =
  { status : Status.t Lwd.var
  ; window : Rb.ro Windows.elt Lwd.var
  ; command : string -> unit
  ; message : string -> unit
  ; quit : unit Lwt.t
  }

type thread =
  [ `Fiber of (unit, Cri_lwt.error) result Lwt.t | `Multiplex of unit Lwt.t ]

type t =
  { command : string Lwt_stream.t
  ; message : string Lwt_stream.t
  ; do_quit : unit Lwt.t * unit Lwt.u
  ; sleep : float -> unit Lwt.t
  ; windows : Windows.t
  ; status : Status.t Lwd.var
  ; ctx : Mimic.ctx
  ; mutable username : Art.key
  ; threads : (Uid.t, thread) Hashtbl.t
  ; servers : (Address.t, Server.t) Hashtbl.t
  ; mutable switchs : Lwt_switch.t list
  }

let make ~ctx ~now ~sleep username =
  let windows = Windows.make ~now username in
  let command, pushc = Lwt_stream.create () in
  let message, pushm = Lwt_stream.create () in
  let quit, do_quit = Lwt.task () in
  let result =
    { status = Lwd.var `None
    ; window = windows.Windows.current
    ; command = (fun str -> pushc (Some str))
    ; message = (fun str -> pushm (Some str))
    ; quit
    }
  in
  ( result
  , { command
    ; message
    ; do_quit = (quit, do_quit)
    ; sleep
    ; windows
    ; status = result.status
    ; ctx
    ; username
    ; threads = Hashtbl.create 0x10
    ; servers = Hashtbl.create 0x10
    ; switchs = []
    } )

module Windows = struct
  let push_on_console t ?prefix msg =
    Windows.push_on_console t.windows ?prefix msg

  let push t msg = Windows.push t.windows ~nickname:t.username msg
end

module Connect = struct
  let dst : Address.t Mimic.value = Mimic.make ~name:"irc-destination"
  let tls : bool Mimic.value = Mimic.make ~name:"irc-tls"
  let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

  let with_destination_and_parameters ctx dst_v _ps =
    let tls_v = true in
    ctx |> Mimic.add dst dst_v |> Mimic.add tls tls_v

  let parse_dst ?port:(default = 6697) str =
    let[@warning "-8"] (port :: rest) =
      match List.rev (String.split_on_char ':' str) with
      | [ host ] -> [ Int.to_string default; host ]
      | _ :: _ as result -> result
      | [] -> assert false
    in
    let host = String.concat ":" (List.rev rest) in
    match
      ( Result.bind (Domain_name.of_string host) Domain_name.host
      , Ipaddr.with_port_of_string ~default (host ^ ":" ^ port)
      , int_of_string_opt port )
    with
    | Ok host, _, Some port -> Ok (`Domain (host, port))
    | Error _, Ok (ipaddr, port), _ -> Ok (`Inet (ipaddr, port))
    | _ -> error_msgf "Invalid destination: %S" str

  let parse_parameter = function
    | "+tls" -> Ok (`TLS true)
    | "-tls" -> Ok (`TLS false)
    | parameter -> (
        match String.split_on_char ':' parameter with
        | "password" :: value -> Ok (`Password (String.concat ":" value))
        | _ -> error_msgf "Invalid parameter: %S" parameter)

  let parse parameters =
    match parameters with
    | [ host ] -> Result.map (fun dst -> (dst, [])) (parse_dst host)
    | host :: parameters ->
        let ( let* ) x f = Result.bind x f in
        let* dst = parse_dst host in
        let parameters = List.map parse_parameter parameters in
        let parameters, _ =
          List.partition_map
            (function
              | Ok v -> Either.Left v
              | Error (`Msg err) ->
                  Log.warn (fun m -> m "%s" err);
                  Either.Right ())
            parameters
        in
        Ok (dst, parameters)
    | [] -> error_msgf "The connect command requires a destination"

  let multiplex t ~close recv =
    `Multiplex
      (let rec loop () =
         recv () >>= function
         | Some (prefix, msg) ->
             let prefix =
               Option.map (Fmt.to_to_string Cri.Protocol.pp_prefix) prefix
             in
             Windows.push_on_console t ?prefix
               (Fmt.str "%a" Cri.Protocol.pp_message msg)
             >>= loop
         | None ->
             close ();
             Lwt.return_unit
       in
       loop ())
end

module Server = struct
  (* TODO(dinosaure): clean [t.threads] when we got an error? *)
  let handle_error_on_cri_thread ~address t = function
    | Ok () -> ()
    | Error
        (`Write _ | `Cycle | `Not_found | `End_of_input | `Msg _ | `Decoder _)
      ->
        Status.errorf "Impossible to connect to %a" Address.pp address
        |> Lwd.set t.status
    | Error `Time_out ->
        Status.errorf "The waiting period has expired for %a" Address.pp address
        |> Lwd.set t.status
    | Error `No_enough_space ->
        Status.errorf "Internal error while connecting to %a" Address.pp address
        |> Lwd.set t.status

  let connect ~address ~parameters ?tls t =
    let timeout () = t.sleep 10. in
    let stop = Lwt_switch.create () in
    let th_loading, wk = Lwt.task () in
    Lwt.async (fun () ->
        Status.loading ~sleep:t.sleep ~stop:th_loading ~text:"Connecting..."
          t.status);
    let connected () =
      Lwt.wakeup_later wk ();
      Lwd.set t.status (`Done "Connected!");
      Lwt.return_unit
    in
    let `Fiber th, recv, _send, close =
      Cri_lwt.run ~connected ~stop ~timeout
        (Connect.with_destination_and_parameters t.ctx address parameters)
    in
    let server = Server.make ?tls address in
    Hashtbl.add t.threads (Server.uid_of_connection server) (`Fiber th);
    Hashtbl.add t.threads
      (Server.uid_of_multiplex server)
      (Connect.multiplex t ~close recv);
    Lwt.on_success th (handle_error_on_cri_thread ~address t);
    t.switchs <- stop :: t.switchs;
    server

  include Server
end

module Command = struct
  let rec process t =
    let* cmd = Lwt_stream.next t.command in
    match Astring.String.cuts ~empty:false ~sep:" " cmd with
    | [ "quit" ] | [ "q" ] ->
        Lwt.wakeup_later (snd t.do_quit) ();
        Lwt.return_unit
    | "connect" :: parameters ->
        (match Connect.parse parameters with
        | Ok (dst, ps) ->
            let server = Server.connect ~address:dst ~parameters:ps t in
            Hashtbl.add t.servers (Server.address server) server;
            Lwt.return_unit
        | Error (`Msg err) ->
            Lwd.set t.status (`Error err);
            Lwt.return_unit)
        >>= fun () -> process t
    | [ "help" ] ->
        let* () = Lwt_list.iter_s (Windows.push_on_console t) Help.text in
        process t
    | _ ->
        Lwd.set t.status (`Error (Fmt.str "Invalid command: %S" cmd));
        process t
end

module Message = struct
  let rec process t =
    let quit =
      let* () = fst t.do_quit in
      Lwt.return `Quit
    in
    let msg =
      let* msg = Lwt_stream.next t.message in
      Lwt.return (`Message msg)
    in
    Lwt.choose [ quit; msg ] >>= function
    | `Quit -> Lwt.return_unit
    | `Message msg ->
        let* () = Windows.push t msg in
        process t
end

let process t =
  Lwt.join [ Command.process t; Message.process t ] >>= fun () ->
  Lwt_list.iter_p Lwt_switch.turn_off t.switchs >>= fun () ->
  Hashtbl.fold (fun uid th acc -> (uid, th) :: acc) t.threads []
  |> Lwt_list.iter_s @@ function
     | _uid, `Multiplex th -> th
     | _uid, `Fiber th -> (
         th >>= function
         | Ok () -> Lwt.return_unit
         | Error err ->
             Log.err (fun m ->
                 m "Got an error at an IRC finalizer: %a" Cri_lwt.pp_error err);
             Lwt.return_unit)
