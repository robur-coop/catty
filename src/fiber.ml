let src = Logs.Src.create "kit.fiber"

module Log = (val Logs.src_log src : Logs.LOG)

type server =
  { stop : Lwt_switch.t
  ; thread : (unit, Cri_lwt.error) result Lwt.t
  ; recv : Cri_lwt.recv
  ; send : Cri_lwt.send
  ; tasks : Task.state list
  ; server : Server.t
  }

module Set = Set.Make (struct
  type t = server

  let compare { server = a; _ } { server = b; _ } = Server.compare a b
end)

type t = { quit : unit Lwt.t; servers : Set.t }

and msg = Task.msg =
  | Message :
      { prefix : Cri.Protocol.prefix option; message : Cri.Protocol.message }
      -> msg

let add_server { quit; servers } ~stop ~thread ~recv ~send server tasks =
  let server = { stop; thread; recv; send; tasks; server } in
  { quit; servers = Set.add server servers }

let servers { servers; _ } =
  Set.elements servers |> List.map (fun { server; _ } -> server)

let servers t = servers t |> Server.Set.of_list |> Server.to_map

let stop_all_tasks tasks =
  let open Lwt.Infix in
  Lwt_list.fold_left_s
    (fun msgs task ->
      let (Task.Value (state, (module State), _inj)) = Task.prj task in
      State.stop state >|= List.rev_append msgs)
    [] tasks

let replace_task :
    type a. on:Server.t -> a Task.witness * a -> t -> (t * msg list) Lwt.t =
 fun ~on (witness, task) ({ servers; _ } as root) ->
  let open Lwt.Infix in
  let open Lwt.Syntax in
  let* msgs, servers =
    Lwt_list.fold_left_s
      (fun (msgs, servers) ({ server; tasks; _ } as v) ->
        if Server.equal server on then
          Lwt_list.fold_left_s
            (fun (msgs, tasks) task' ->
              match Task.prove witness task' with
              | Some _ ->
                  let (Task.Value (state, (module State), _inj)) =
                    Task.prj task'
                  in
                  State.stop state >>= fun msgs' ->
                  Lwt.return
                    (List.rev_append msgs msgs', Task.v witness task :: tasks)
              | None -> Lwt.return (msgs, task' :: tasks))
            (msgs, []) tasks
          >>= fun (msgs', tasks) ->
          Lwt.return (List.rev_append msgs msgs', { v with tasks } :: servers)
        else Lwt.return (msgs, v :: servers))
      ([], []) (Set.elements servers)
  in
  Lwt.return ({ root with servers = Set.of_list servers }, msgs)

let tasks_of_server ~on { servers; _ } =
  match
    Set.find_first_opt (fun { server; _ } -> Server.equal server on) servers
  with
  | Some { tasks; _ } -> tasks
  | None -> []

let send_msgs send =
  List.iter (fun (Message { prefix; message = Cri.Protocol.Message (w, v) }) ->
      send.Cri_lwt.send ?prefix w v)

let merge { servers = a; quit } { servers = b; _ } =
  { quit; servers = Set.union a b }

let process ({ servers; quit } as root) =
  let open Lwt.Infix in
  match Lwt.state quit with
  | Return () | Fail _ ->
      let f1 msgs task =
        let (Task.Value (state, (module State), _inj)) = Task.prj task in
        State.stop state >|= List.rev_append msgs
      in
      let f0 { thread; tasks; send; stop; _ } =
        Lwt_list.fold_left_s f1 [] tasks >>= fun msgs ->
        send_msgs send msgs;
        Lwt_switch.turn_off stop >>= fun () ->
        thread >>= fun _res -> Lwt.return_unit
      in
      Lwt_list.iter_p f0 (Set.elements servers) >>= fun () -> Lwt.return_none
  | Sleep ->
      let f1 (prefix, v) (msgs, tasks) task =
        let (Task.Value (state, (module State), inj)) = Task.prj task in
        State.recv state ?prefix v >>= function
        | `Stop msgs' -> Lwt.return (List.rev_append msgs msgs', tasks)
        | `Continue (msgs', state) ->
            Lwt.return (List.rev_append msgs msgs', inj state :: tasks)
      in
      let rec f0 ({ send; recv; tasks; server; _ } as v) =
        Log.debug (fun m ->
            m "Waiting message from %a" Address.pp (Server.address server));
        Lwt.catch
          (fun () ->
            Lwt.pause () >>= recv >>= function
            | Some msg ->
                Lwt_list.fold_left_s (f1 msg) ([], []) tasks
                >>= fun (msgs, tasks) ->
                send_msgs send msgs;
                f0 { v with tasks }
            | None -> Lwt.return_none)
          (fun _exn -> Lwt.return_some { v with tasks })
      in
      Lwt_list.map_p f0 (Set.elements servers) >>= fun servers ->
      let servers = List.filter_map Fun.id servers in
      Lwt.return_some { root with servers = Set.of_list servers }
