open Lwt.Infix
open Lwt.Syntax

let src = Logs.Src.create "kit.engine"

module Log = (val Logs.src_log src : Logs.LOG)

type t =
  { command : string Lwt_stream.t
  ; message : string Lwt_stream.t
  ; action : Action.t -> unit
  ; do_quit : unit -> unit
  ; fibers : (unit, unit) Fiber.t
  ; sleep : float -> unit Lwt.t
  ; now : unit -> Ptime.t
  ; ctx : Mimic.ctx
  ; host : [ `raw ] Domain_name.t
  ; user : User.t
  }

let prefix_of_engine t =
  Cri.Protocol.prefix ~host:t.host (List.hd (User.nicknames t.user))

let make ~ctx ~now ~sleep ~host user =
  let command, pushc = Lwt_stream.create () in
  let message, pushm = Lwt_stream.create () in
  let action, pusha = Lwt_stream.create () in
  let quit, do_quit = Lwt.task () in
  let do_quit () =
    pusha None;
    Lwt.wakeup_later do_quit ()
  in
  ( (quit, (fun str -> pushc (Some str)), (fun str -> pushm (Some str)), action)
  , { command
    ; message
    ; action = (fun action -> pusha (Some action))
    ; do_quit
    ; fibers = Fiber.Root { quit; servers = [] }
    ; sleep
    ; now
    ; ctx
    ; user
    ; host
    } )

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
end

module Server = struct
  let connect ~address ~parameters t =
    Log.debug (fun m -> m "Connect to %a." Address.pp address);
    let timeout () = t.sleep (Duration.of_sec 255 |> Duration.to_f) in
    let stop = Lwt_switch.create () in
    let th_loading, wk = Lwt.task () in
    Lwt.async (fun () ->
        Status.loading ~sleep:t.sleep ~stop:th_loading ~text:"Connecting..."
          (fun status -> t.action (Action.set_status status)));
    let connected () =
      Lwt.wakeup_later wk ();
      t.action (Action.set_status (`Done "Connected!"));
      Lwt.return_unit
    in
    let `Fiber thread, recv, send, _close (* TODO? *) =
      Cri_lwt.run ~connected ~stop ~timeout
        (Connect.with_destination_and_parameters t.ctx address parameters)
    in
    let server = Server.make ~tls:true address send in
    let prefix = prefix_of_engine t in
    let open Lwt.Syntax in
    let* tasks, msgs =
      let* ping, msgs0 = Task.Ping.v ~prefix () in
      let* nick, msgs1 =
        State.Nickname.v ~prefix ~now:t.now ~action:t.action server
          (User.nicknames t.user)
      in
      let* error, msgs2 = State.Error.v ~now:t.now ~action:t.action server in
      let* notice, msgs3 = State.Notice.v ~now:t.now ~action:t.action server in
      Lwt.return
        ( [ Fiber.Task (Task.v Task.ping ping)
          ; Fiber.Task (Task.v State.nickname nick)
          ; Fiber.Task (Task.v State.error error)
          ; Fiber.Task (Task.v State.notice notice)
          ]
        , List.concat [ msgs0; msgs1; msgs2; msgs3 ] )
    in
    send.send ~prefix Cri.Protocol.User (User.user t.user);
    List.iter
      (fun (Fiber.Message { prefix; message = Cri.Protocol.Message (w, v) }) ->
        send.Cri_lwt.send ?prefix w v)
      msgs;
    Log.debug (fun m -> m "Server %a added!" Server.pp server);
    Lwt.return
      { t with
        fibers =
          Fiber.add_server t.fibers ~stop ~thread ~recv ~send server tasks
      }

  include Server
end

module Command = struct
  let process t =
    let* cmd = Lwt_stream.next t.command in
    match Astring.String.cuts ~empty:false ~sep:" " cmd with
    | [ "quit" ] | [ "q" ] ->
        Log.debug (fun m -> m "Quit the engine.");
        t.do_quit ();
        Lwt.return t
    | "connect" :: parameters -> (
        Log.debug (fun m -> m "Got a connect command.");
        match Connect.parse parameters with
        | Ok (dst, ps) -> Server.connect ~address:dst ~parameters:ps t
        | Error (`Msg err) ->
            t.action (Action.set_status (`Error err));
            Lwt.return t)
    | [ "help" ] ->
        (* let* () = Lwt_list.iter_s (Windows.push_on_console t) Help.text in *)
        Lwt.return t
    | _ ->
        t.action (Action.set_status (Status.errorf "Invalid command: %S" cmd));
        Lwt.return t

  let run t =
    let open Lwt.Infix in
    process t >|= fun t -> `Yield t
end

module Message = struct
  let process t = Lwt_stream.next t.message >>= fun _msg -> Lwt.return t

  let run t =
    let open Lwt.Infix in
    process t >|= fun t -> `Yield t
end

module Fiber = struct
  let run t =
    Fiber.process t.fibers >>= function
    | Some fibers -> Lwt.return (`Yield { t with fibers })
    | None -> Lwt.return `Quit
end

module Ui = struct end

let process t =
  let rec go t =
    (* NOTE(dinosaure): must be [Lwt.pick] which cancels threads! *)
    Lwt.pick [ Command.run t; Message.run t; Fiber.run t ] >>= function
    | `Yield t -> Lwt.pause () >>= fun () -> go t
    | `Quit ->
        Log.debug (fun m -> m "The engine is finished.");
        Lwt.return_unit
  in
  go t
