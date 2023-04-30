let src = Logs.Src.create "kit.fiber"

module Log = (val Logs.src_log src : Logs.LOG)

type ('i, 'o) t =
  | Root :
      { quit : unit Lwt.t
      ; servers : ([ `Disconnect | `Continue ], unit) t list
      }
      -> (unit, unit) t
  | Server :
      { stop : Lwt_switch.t
      ; thread : (unit, Cri_lwt.error) result Lwt.t
      ; recv : Cri_lwt.recv
      ; send : Cri_lwt.send
      ; tasks : (msg, msg list) t list
      ; uid : Uid.t
      }
      -> ([ `Disconnect | `Continue ], unit) t
  | Task : Task.state -> (msg, msg list) t

and msg = Task.msg =
  | Message :
      { prefix : Cri.Protocol.prefix option; message : Cri.Protocol.message }
      -> msg

let add_server (Root { quit; servers }) ~stop ~thread ~recv ~send server tasks =
  let server =
    Server { stop; thread; recv; send; tasks; uid = Server.uid server }
  in
  Root { quit; servers = server :: servers }

let stop_all_tasks tasks =
  let open Lwt.Infix in
  Lwt_list.fold_left_s
    (fun msgs (Task task) ->
      let (Task.Value (state, (module State), _inj)) = Task.prj task in
      State.stop state >|= List.rev_append msgs)
    [] tasks

let process t =
  let open Lwt.Infix in
  let rec go : type i o. i -> (i, o) t -> (o * (i, o) t option) Lwt.t =
   fun v w ->
    match (w, v) with
    | Root { quit; servers }, () -> (
        match Lwt.state quit with
        | Sleep ->
            Lwt_list.fold_left_s
              (fun servers server ->
                go `Continue server >|= function
                | (), Some server -> server :: servers
                | (), None -> servers)
              [] servers
            >>= fun servers -> Lwt.return ((), Some (Root { quit; servers }))
        | Return () | Fail _ ->
            Lwt_list.iter_p
              (fun server -> go `Disconnect server >|= fst)
              servers
            >>= fun () -> Lwt.return ((), None))
    | Server { stop; send; thread; tasks; _ }, `Disconnect ->
        Lwt_list.fold_left_s
          (fun msgs (Task state) ->
            let (Task.Value (state, (module State), _inj)) = Task.prj state in
            State.stop state >|= List.rev_append msgs)
          [] tasks
        >>= fun msgs ->
        List.iter
          (fun (Message { prefix; message = Cri.Protocol.Message (w, v) }) ->
            send.Cri_lwt.send ?prefix w v)
          msgs;
        Lwt_switch.turn_off stop >>= fun () ->
        thread >>= fun _res -> Lwt.return ((), None)
    | Server { stop; recv; send; thread; tasks; uid }, `Continue -> (
        recv () >>= fun msg ->
        match (Lwt.state thread, msg) with
        | Return (Ok ()), _ ->
            stop_all_tasks tasks >>= fun msgs ->
            Log.debug (fun m ->
                m "The IRC server connection was closed (%d messages was left)"
                  (List.length msgs));
            (* XXX(dinosaure): the socket was closed properly,
               we are not able to send messages from tasks. *)
            Lwt.return ((), None)
        | Return (Error err), _ ->
            Log.err (fun m ->
                m "Got an error from the IRC server connection: %a"
                  Cri_lwt.pp_error err);
            stop_all_tasks tasks >>= fun _msgs -> Lwt.return ((), None)
        | Fail exn, _ ->
            Log.err (fun m ->
                m "Got an exception from the IRC server connection: %s"
                  (Printexc.to_string exn));
            stop_all_tasks tasks >>= fun _msgs -> Lwt.return ((), None)
        | Sleep, None ->
            stop_all_tasks tasks >>= fun msgs ->
            List.iter
              (fun (Message { prefix; message = Cri.Protocol.Message (w, v) }) ->
                send.Cri_lwt.send ?prefix w v)
              msgs;
            Lwt_switch.turn_off stop >>= fun () ->
            (* TODO(dinosaure): close? *)
            Lwt.return ((), None)
        | Sleep, Some (prefix, message) -> (
            let msg = Message { prefix; message } in
            Lwt_list.fold_left_s
              (fun (msgs, tasks) task ->
                go msg task >|= function
                | msgs', Some task -> (List.rev_append msgs' msgs, task :: tasks)
                | msgs', None -> (List.rev_append msgs' msgs, tasks))
              ([], []) tasks
            >>= function
            | msgs, [] ->
                List.iter
                  (fun (Message
                         { prefix; message = Cri.Protocol.Message (w, v) }) ->
                    send.Cri_lwt.send ?prefix w v)
                  msgs;
                Lwt_switch.turn_off stop >>= fun () ->
                thread >>= fun _res -> Lwt.return ((), None)
            | msgs, tasks ->
                List.iter
                  (fun (Message
                         { prefix; message = Cri.Protocol.Message (w, v) }) ->
                    send.Cri_lwt.send ?prefix w v)
                  msgs;
                Lwt.return
                  ((), Some (Server { stop; recv; send; thread; tasks; uid }))))
    | Task task, Message { prefix; message } -> (
        let (Task.Value (state, (module State), inj)) = Task.prj task in
        State.recv state ?prefix message >>= function
        | `Stop msgs -> Lwt.return (msgs, None)
        | `Continue (msgs, state) -> Lwt.return (msgs, Some (Task (inj state))))
  in
  go () t >>= fun ((), v) -> Lwt.return v
