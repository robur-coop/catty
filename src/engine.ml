open Lwt.Infix
open Lwt.Syntax

let src = Logs.Src.create "kit.engine"

module Log = (val Logs.src_log src : Logs.LOG)

type vars = {
    status: Status.t Lwd.var
  ; window: Rb.ro Windows.elt Lwd.var
  ; command: string -> unit
  ; message: string -> unit
  ; quit: unit Lwt.t
}

type thread =
  [ `Fiber of (unit, Cri_lwt.error) result Lwt.t | `Multiplex of unit Lwt.t ]

type t = {
    command: string Lwt_stream.t
  ; message: string Lwt_stream.t
  ; do_quit: unit Lwt.t * unit Lwt.u
  ; sleep: float -> unit Lwt.t
  ; windows: Windows.t
  ; status: Status.t Lwd.var
  ; ctx: Mimic.ctx
  ; mutable username: Art.key
  ; mutable threads: thread list
  ; mutable switchs: Lwt_switch.t list
}

let make ~ctx ~now ~sleep username =
  let windows = Windows.make ~now username in
  let command, pushc = Lwt_stream.create () in
  let message, pushm = Lwt_stream.create () in
  let quit, do_quit = Lwt.task () in
  let result =
    {
      status= Lwd.var `None
    ; window= windows.Windows.current
    ; command= (fun str -> pushc (Some str))
    ; message= (fun str -> pushm (Some str))
    ; quit
    } in
  ( result
  , {
      command
    ; message
    ; do_quit= quit, do_quit
    ; sleep
    ; windows
    ; status= result.status
    ; ctx
    ; username
    ; threads= []
    ; switchs= []
    } )

module Status = struct
  let loading =
    let tbl =
      [|
         Uchar.of_int 0x280B (* "⠋" *); Uchar.of_int 0x2819 (* "⠙" *)
       ; Uchar.of_int 0x2839 (* "⠹" *); Uchar.of_int 0x2838 (* "⠸" *)
       ; Uchar.of_int 0x283C (* "⠼" *); Uchar.of_int 0x2834 (* "⠴" *)
       ; Uchar.of_int 0x2826 (* "⠦" *); Uchar.of_int 0x2827 (* "⠧" *)
       ; Uchar.of_int 0x2807 (* "⠇" *); Uchar.of_int 0x280F (* "⠏" *)
      |] in
    fun ~stop ~text t ->
      let stop () =
        let+ () = stop in
        `Stop in
      let continue () =
        let+ () = t.sleep 0.1 in
        `Continue in
      let rec go idx =
        if idx = Array.length tbl then go 0
        else (
          Lwd.set t.status (`Progress (tbl.(idx), text))
          ; Lwt.choose [continue (); stop ()] >>= function
            | `Continue -> go (succ idx)
            | `Stop -> Lwt.return_unit) in
      go 0
end

module Windows = struct
  let push_on_console t ?prefix msg =
    Windows.push_on_console t.windows ?prefix msg

  let push t msg = Windows.push t.windows ~nickname:t.username msg
end

module Connect = struct
  let dst :
      [ `Domain of [ `host ] Domain_name.t * int | `Inet of Ipaddr.t * int ]
      Mimic.value =
    Mimic.make ~name:"irc-destination"

  let tls : bool Mimic.value = Mimic.make ~name:"irc-tls"
  let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

  let pp ppf = function
    | `Domain (host, port) -> Fmt.pf ppf "%a:%d" Domain_name.pp host port
    | `Inet (ipaddr, port) -> Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port

  let with_destination_and_parameters ctx dst_v _ps =
    let tls_v = true in
    ctx |> Mimic.add dst dst_v |> Mimic.add tls tls_v

  let parse_dst ?port:(default = 6697) str =
    let[@warning "-8"] (port :: rest) =
      match List.rev (String.split_on_char ':' str) with
      | [host] -> [Int.to_string default; host]
      | _ :: _ as result -> result
      | [] -> assert false in
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
    | [host] -> Result.map (fun dst -> dst, []) (parse_dst host)
    | host :: parameters ->
      let ( let* ) x f = Result.bind x f in
      let* dst = parse_dst host in
      let parameters = List.map parse_parameter parameters in
      let parameters, _ =
        List.partition_map
          (function
            | Ok v -> Either.Left v
            | Error (`Msg err) ->
              Log.warn (fun m -> m "%s" err)
              ; Either.Right ())
          parameters in
      Ok (dst, parameters)
    | [] -> error_msgf "The connect command requires a destination"

  let multiplex t ~close recv =
    `Multiplex
      (let rec loop () =
         recv () >>= function
         | Some (prefix, msg) ->
           let prefix =
             Option.map (Fmt.to_to_string Cri.Protocol.pp_prefix) prefix in
           Windows.push_on_console t ?prefix
             (Fmt.str "%a" Cri.Protocol.pp_message msg)
           >>= loop
         | None -> close () ; Lwt.return_unit in
       loop ())
end

module Command = struct
  let rec process t =
    let* cmd = Lwt_stream.next t.command in
    match Astring.String.cuts ~empty:false ~sep:" " cmd with
    | ["quit"] | ["q"] ->
      Lwt.wakeup_later (snd t.do_quit) ()
      ; Lwt.return_unit
    | "connect" :: parameters ->
      (match Connect.parse parameters with
      | Ok (dst, ps) ->
        let timeout () = t.sleep 1. in
        let stop = Lwt_switch.create () in
        let th_loading, wk = Lwt.task () in
        Lwt.async (fun () ->
            Status.loading ~stop:th_loading ~text:"Connecting..." t)
        ; let th, recv, _send, close =
            Cri_lwt.run ~stop ~timeout
              (Connect.with_destination_and_parameters t.ctx dst ps) in
          Lwt.wakeup_later wk ()
          ; Lwd.set t.status (`Done "Connected!")
          ; t.threads <-
              Connect.multiplex t ~close recv :: (th :> thread) :: t.threads
          ; t.switchs <- stop :: t.switchs
          ; Lwt.return_unit
      | Error (`Msg err) ->
        Lwd.set t.status (`Error err)
        ; Lwt.return_unit)
      >>= fun () -> process t
    | ["help"] ->
      let* () = Lwt_list.iter_s (Windows.push_on_console t) Help.text in
      process t
    | _ ->
      Lwd.set t.status (`Error (Fmt.str "Invalid command: %S" cmd))
      ; process t
end

module Message = struct
  let rec process t =
    let quit =
      let* () = fst t.do_quit in
      Lwt.return `Quit in
    let msg =
      let* msg = Lwt_stream.next t.message in
      Lwt.return (`Message msg) in
    Lwt.choose [quit; msg] >>= function
    | `Quit -> Lwt.return_unit
    | `Message msg ->
      let* () = Windows.push t msg in
      process t
end

let process t =
  Lwt.join [Command.process t; Message.process t] >>= fun () ->
  Lwt_list.iter_p Lwt_switch.turn_off t.switchs >>= fun () ->
  t.threads
  |> Lwt_list.iter_s @@ function
     | `Multiplex th -> th
     | `Fiber th -> (
       th >>= function
       | Ok () -> Lwt.return_unit
       | Error err ->
         Log.err (fun m ->
             m "Got an error at an IRC finalizer: %a" Cri_lwt.pp_error err)
         ; Lwt.return_unit)
