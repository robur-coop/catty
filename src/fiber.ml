let src = Logs.Src.create "kit.fiber"

module Log = (val Logs.src_log src : Logs.LOG)

type server =
  { stop : Lwt_switch.t
  ; thread : (unit, Cri_lwt.error) result Lwt.t
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

let empty ~quit = { quit; servers = Set.empty }
let server { server; _ } = server

let add_server { quit; servers } ~stop ~thread ~send server tasks =
  let server = { stop; thread; send; tasks; server } in
  { quit; servers = Set.add server servers }

let servers { servers; _ } =
  Set.elements servers |> List.map (fun { server; _ } -> server)

let servers t = servers t |> Server.Set.of_list |> Server.to_map

(*
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
*)

let tasks_of_server ~on { servers; _ } =
  Log.debug (fun m ->
      m "Search tasks of %a (name: %s)" Address.pp (Server.address on)
        (Server.name on));
  match
    List.find_opt
      (fun { server; _ } -> Server.equal server on)
      (Set.elements servers)
  with
  | Some { tasks; _ } -> tasks
  | None -> []

let send_msgs send =
  List.iter (fun (Message { prefix; message = Cri.Protocol.Message (w, v) }) ->
      send.Cri_lwt.send ?prefix w v)

let add_task : on:Server.t -> Task.state * Task.msg list -> t -> t Lwt.t =
 fun ~on (task, msgs) ({ servers; _ } as root) ->
  let open Lwt.Syntax in
  let+ servers =
    Lwt_list.map_p
      (fun ({ send; tasks; server; _ } as v) ->
        if Server.equal server on then (
          send_msgs send msgs;
          Lwt.return { v with tasks = task :: tasks })
        else Lwt.return v)
      (Set.elements servers)
  in
  { root with servers = Set.of_list servers }

let send_of_server ~on { servers; _ } =
  List.filter_map
    (fun { send; server; _ } ->
      if Server.equal server on then Some send else None)
    (Set.elements servers)
  |> function
  | [] -> None
  | x :: _ -> Some x

let stop { servers; _ } =
  let open Lwt.Infix in
  Log.debug (fun m -> m "Quit properly opened servers.");
  let f1 msgs task =
    let (Task.Value (state, (module State), _inj)) = Task.prj task in
    State.stop state >|= List.rev_append msgs
  in
  let f0 { tasks; send; stop; server; _ } =
    Lwt_list.fold_left_s f1 [] tasks >>= fun msgs ->
    send_msgs send msgs;
    Log.debug (fun m -> m "Start to stop %a" Address.pp (Server.address server));
    Lwt_switch.turn_off stop >>= fun () ->
    (* thread >>= fun _res -> *)
    Log.debug (fun m -> m "%a is stopped" Address.pp (Server.address server));
    Lwt.return_unit
  in
  Lwt_list.iter_s f0 (Set.elements servers)

let process ~on (prefix, v) ({ servers; _ } as root) =
  let open Lwt.Infix in
  let f1 (msgs, tasks) task =
    let (Task.Value (state, (module State), inj)) = Task.prj task in
    State.recv state ?prefix v >>= function
    | `Stop msgs' -> Lwt.return (List.rev_append msgs msgs', tasks)
    | `Continue (msgs', state) ->
        Lwt.return (List.rev_append msgs msgs', inj state :: tasks)
  in
  let f0 ({ send; tasks; server; _ } as sv) =
    if Server.equal server on then
      Lwt.catch
        (fun () ->
          Lwt_list.fold_left_s f1 ([], []) tasks >>= fun (msgs, tasks) ->
          send_msgs send msgs;
          Lwt.return { sv with tasks })
        (fun _exn -> Lwt.return sv)
    else Lwt.return sv
  in
  Lwt_list.map_p f0 (Set.elements servers) >>= fun servers ->
  Lwt.return_some { root with servers = Set.of_list servers }
