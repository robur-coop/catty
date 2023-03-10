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

type t = {
    command: string Lwt_stream.t
  ; message: string Lwt_stream.t
  ; do_quit: unit Lwt.t * unit Lwt.u
  ; sleep: float -> unit Lwt.t
  ; windows: Windows.t
  ; status: Status.t Lwd.var
}

let make ~now ~sleep () =
  let windows = Windows.make ~now in
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
  let push_on_console t msg = Windows.push_on_console t.windows msg
end

module Command = struct
  let rec process t =
    let* cmd = Lwt_stream.next t.command in
    match Astring.String.cuts ~empty:false ~sep:" " cmd with
    | ["quit"] | ["q"] ->
      Lwt.wakeup_later (snd t.do_quit) ()
      ; Lwt.return_unit
    | ["connect"] ->
      let stop, do_stop = Lwt.task () in
      Lwt.async (fun () -> Status.loading ~stop ~text:"loading..." t)
      ; let* () = t.sleep 5.0 in
        (* TODO(dinosaure): blocking connect! *)
        Lwt.wakeup_later do_stop ()
        ; Lwd.set t.status (`Done "Connected!")
        ; process t
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
    | `Message _msg -> process t
end

let process t = Lwt.join [Command.process t; Message.process t]
