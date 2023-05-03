open Task

let src = Logs.Src.create "kit.tasks"

module Log = (val Logs.src_log src : Logs.LOG)

module Nickname = struct
  type t =
    { candidates : Cri.Nickname.t list
    ; prefix : Cri.Protocol.prefix option
    ; action : Action.t -> unit
    ; server : Server.t
    ; now : unit -> Ptime.t
    }

  let recv t ?prefix (Cri.Protocol.Message (cmd, v)) =
    match (cmd, v) with
    | Cri.Protocol.ERR_NICKNAMEINUSE, nickname -> (
        t.action
          (Action.new_message ~uid:Uid.console
             (Message.msgf ~now:t.now ?prefix ~server:t.server
                "The nickname %a is already taken" Cri.Nickname.pp nickname));

        let candidates =
          List.filter
            (fun n' -> Cri.Nickname.equal nickname n' = false)
            t.candidates
        in
        match candidates with
        | [] ->
            t.action
              (Action.set_status
                 (Status.errorf "All nicknames are taken, close the connection"));
            let msg =
              Message
                { prefix = t.prefix
                ; message =
                    Message (Cri.Protocol.Quit, "All nicknames are taken")
                }
            in
            Lwt.return (`Stop [ msg ])
        | nick :: _ ->
            let msg =
              Message
                { prefix = t.prefix
                ; message =
                    Message
                      (Cri.Protocol.Nick, { Cri.Protocol.nick; hopcount = None })
                }
            in
            Lwt.return (`Continue ([ msg ], { t with candidates })))
    | _ -> Lwt.return (`Continue ([], t))

  let stop _ = Lwt.return []

  let current_nickname t =
    match t.candidates with
    | nickname :: _ -> nickname
    | [] -> failwith "Invalid Nickname state (no candidates)"

  let v ~now ~action ?prefix server = function
    | nick :: _ as candidates ->
        let msg =
          Message
            { prefix
            ; message =
                Message
                  (Cri.Protocol.Nick, { Cri.Protocol.nick; hopcount = None })
            }
        in
        Lwt.return ({ prefix; action; candidates; now; server }, [ msg ])
    | [] -> invalid_arg "The Nickname state requires at least one nickname"
end

module Error = struct
  type t =
    { now : unit -> Ptime.t; action : Action.t -> unit; server : Server.t }

  let recv ({ now; action; server } as t) ?prefix
      (Cri.Protocol.Message (cmd, v)) =
    match (cmd, v) with
    | Cri.Protocol.Error, Some str ->
        action
          (Action.new_message ~uid:Uid.console
             (Message.msgf ~now ?prefix ~server "%s" str));
        Lwt.return (`Continue ([], t))
    | _ -> Lwt.return (`Continue ([], t))

  let stop _ = Lwt.return []
  let v ~now ~action server = Lwt.return ({ now; action; server }, [])
end

module Notice = struct
  let src = Logs.Src.create "kit.notice"

  module Log = (val Logs.src_log src : Logs.LOG)

  type t =
    { now : unit -> Ptime.t; action : Action.t -> unit; server : Server.t }

  let recv ({ now; action; server } as t) ?prefix
      (Cri.Protocol.Message (cmd, v)) =
    match (cmd, v) with
    | Cri.Protocol.Notice, { Cri.Protocol.dsts = [ dst ]; msg } ->
        if Cri.Destination.everywhere dst then (
          Log.debug (fun m -> m "Got a message to everywhere: %S" msg);
          action
            (Action.new_message ~uid:Uid.console
               (Message.msgf ~now ?prefix ~server "%s" msg));
          Lwt.return (`Continue ([], t)))
        else Lwt.return (`Continue ([], t))
    | Cri.Protocol.RPL_MOTDSTART, Some msg ->
        Log.debug (fun m -> m "Got a MOTD_START message: %S" msg);
        action
          (Action.new_message ~uid:Uid.console
             (Message.msgf ~now ?prefix ~server "%s" msg));
        Lwt.return (`Continue ([], t))
    | Cri.Protocol.RPL_MOTD, (`Pretty msg | `String msg) ->
        Log.debug (fun m -> m "Got a MOTD message: %S" msg);
        action
          (Action.new_message ~uid:Uid.console
             (Message.msgf ~now ?prefix ~server "%s" msg));
        Lwt.return (`Continue ([], t))
    | _ -> Lwt.return (`Continue ([], t))

  let stop _ = Lwt.return []
  let v ~now ~action server = Lwt.return ({ now; action; server }, [])
end

open Lwt.Syntax

let nickname_witness = Task.inj (module Nickname)
let error_witness = Task.inj (module Error)
let notice_witness = Task.inj (module Notice)

let nickname ~now ~action ?prefix server nicknames =
  let* state, msgs = Nickname.v ~now ~action ?prefix server nicknames in
  Lwt.return (Task.v nickname_witness state, msgs)

let error ~now ~action server =
  let* state, msgs = Error.v ~now ~action server in
  Lwt.return (Task.v error_witness state, msgs)

let notice ~now ~action server =
  let* state, msgs = Notice.v ~now ~action server in
  Lwt.return (Task.v notice_witness state, msgs)

let current_nickname tasks =
  List.map (Task.prove nickname_witness) tasks
  |> List.find_opt Option.is_some
  |> Option.join
  |> Option.map Nickname.current_nickname
  |> function
  | None ->
      Log.err (fun m -> m "The nickname task does not exist for the given list");
      raise Not_found
  | Some nickname -> nickname
