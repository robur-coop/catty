let src = Logs.Src.create "kit.engine"

module Log = (val Logs.src_log src : Logs.LOG)

type t =
  { command : (Uid.t * string) Lwt_stream.t
  ; message : (Uid.t * string) Lwt_stream.t
  ; action : Action.t -> unit
  ; do_quit : unit -> unit
  ; fibers : Fiber.t
  ; sleep : float -> unit Lwt.t
  ; now : unit -> Ptime.t
  ; ctx : Mimic.ctx
  ; host : [ `raw ] Domain_name.t
  ; user : User.t
  ; windows : (Uid.t, string) Hashtbl.t
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
    ; fibers = { quit; servers = Fiber.Set.empty }
    ; sleep
    ; now
    ; ctx
    ; user
    ; host
    ; windows = Hashtbl.create 0x10
    } )

module Connect = struct
  let dst : Address.t Mimic.value = Mimic.make ~name:"irc-destination"
  let tls : bool Mimic.value = Mimic.make ~name:"irc-tls"
  let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

  let with_destination_and_parameters ctx dst_v _ps =
    let tls_v = true in
    ctx |> Mimic.add dst dst_v |> Mimic.add tls tls_v

  let parse_parameter = function
    | parameter -> (
        match String.split_on_char ':' parameter with
        | "password" :: value -> Ok (`Password (String.concat ":" value))
        | "name" :: value -> Ok (`Name (String.concat ":" value))
        | _ -> error_msgf "Invalid parameter: %S" parameter)

  let parse parameters =
    match parameters with
    | [ host ] -> Result.map (fun dst -> (dst, [])) (Address.of_string host)
    | host :: parameters ->
        let ( let* ) x f = Result.bind x f in
        let* dst = Address.of_string host in
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
    let server =
      let name =
        List.filter_map
          (function `Name str -> Some str | _ -> None)
          parameters
        |> function
        | [] -> None
        | name :: _ -> Some name
      in
      Server.make ?name address
    in
    let prefix = prefix_of_engine t in
    let open Lwt.Syntax in
    let* tasks, msgs =
      let open Task in
      let* ping = Task.ping ~prefix ()
      and* nickname =
        Tasks.nickname ~prefix ~now:t.now ~action:t.action server
          (User.nicknames t.user)
      and* error = Tasks.error ~now:t.now ~action:t.action server
      and* notice = Tasks.notice ~now:t.now ~action:t.action server in
      return [ ping; nickname; error; notice ]
    in
    send.send ~prefix Cri.Protocol.User (User.user t.user);
    Fiber.send_msgs send msgs;
    Log.debug (fun m -> m "Server %a added!" Server.pp server);
    Lwt.return
      { t with
        fibers =
          Fiber.add_server t.fibers ~stop ~thread ~recv ~send server tasks
      }

  include Server
end

module Command = struct
  let process t ~uid cmd =
    let servers = Fiber.servers t.fibers in
    match (Uid.to_int uid, Astring.String.cuts ~empty:false ~sep:" " cmd) with
    | _, [ "quit" ] | _, [ "q" ] ->
        Log.debug (fun m -> m "Quit the engine.");
        t.do_quit ();
        Lwt.return t
    | _, "connect" :: parameters -> (
        Log.debug (fun m -> m "Got a connect command.");
        match Connect.parse parameters with
        | Ok (dst, ps) -> Server.connect ~address:dst ~parameters:ps t
        | Error (`Msg err) ->
            t.action (Action.set_status (`Error err));
            Lwt.return t)
    | _, [ "help" ] ->
        (* let* () = list.iter_s (Windows.push_on_console t) Help.text in *)
        Lwt.return t
    | _, [ "whoami"; server ] -> (
        Log.debug (fun m -> m "whoami on %S" server);
        match Server.Map.find_opt server servers with
        | Some server ->
            let tasks = Fiber.tasks_of_server t.fibers ~on:server in
            let nickname = Tasks.current_nickname tasks in
            t.action
              (Action.new_message ~uid:Uid.console
                 (Message.msgf ~now:t.now "Your nickname on %a is %a" Server.pp
                    server Cri.Nickname.pp nickname));
            Lwt.return t
        | None ->
            t.action
              (Action.set_status
                 (Status.errorf "Server %S does not exist" server));
            Lwt.return t)
    | _, [ "nickname"; _nickname ] -> assert false
    | _, [ "server"; "list" ] when Server.Map.cardinal servers = 0 ->
        t.action
          (Action.new_message ~uid:Uid.console
             (Message.msgf ~now:t.now "You are not connected to any servers"));
        Lwt.return t
    | _, [ "server"; "list" ] ->
        t.action
          (Action.new_message ~uid:Uid.console
             (Message.msgf ~now:t.now "%a"
                Fmt.(list ~sep:(const string "\n") (using fst string))
                (Server.Map.bindings servers)));
        Lwt.return t
    | 0, [ "join"; _channel ] when Server.Map.cardinal servers = 1 ->
        assert false
    | 0, [ "join"; _channel ] -> assert false
    | _, [ "join"; _channel; _server ] -> assert false
    | _ ->
        t.action (Action.set_status (Status.errorf "Invalid command: %S" cmd));
        Lwt.return t
end

module Message = struct
  let process t ~uid:_ _msg = Lwt.return t
end

let process t =
  let open Lwt.Infix in
  let on_message t =
    Lwt_stream.next t.message >|= fun (uid, v) -> `Message (uid, v)
  in
  let on_command t =
    Lwt_stream.next t.command >|= fun (uid, v) -> `Command (uid, v)
  in
  let on_fiber t =
    Fiber.process t.fibers >>= function
    | Some fibers -> Lwt.return (`Yield { t with fibers })
    | None -> Lwt.return `Stop
  in

  let rec go t =
    Lwt.pick [ on_message t; on_command t; on_fiber t ] >>= function
    | `Message (uid, v) -> Message.process t ~uid v >>= go
    | `Command (uid, v) ->
        Log.debug (fun m -> m "Got a new command!");
        Command.process t ~uid v >>= go
    | `Yield t -> Lwt.pause () >>= fun () -> go t
    | `Stop ->
        Log.debug (fun m -> m "The engine is finished!");
        Lwt.return_unit
  in
  go t
