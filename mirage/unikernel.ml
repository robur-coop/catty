type state = {
    env: (string, string) Hashtbl.t
  ; sigwinch: (int * int) Lwt_condition.t
  ; mutable size: int * int
}

(* XXX(dinosaure): notable difference between `notty-{unix,lwt}` and `awa-ssh`:
   - The size (and the update of the size) is not given by a signal and a C call
     but from SSH with `Pty_req`/`Pty_set`
*)

open Lwt.Infix
open Lwt.Syntax

module Make
    (Random : Mirage_random.S)
    (Time : Mirage_time.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Stack : Tcpip.Stack.V4V6) =
struct
  module SSH = Awa_mirage.Make (Stack.TCP) (Time) (Mclock)
  module Nottui' = Nottui_mirage.Make (Time)

  let ssh_handler ~stop t = function
    | SSH.Pty_req {width; height; max_width; max_height; term} ->
      t.size <- Int32.to_int width, Int32.to_int height
      ; Lwt.return_unit
    | SSH.Pty_set {width; height; max_width; max_height} ->
      Lwt_condition.broadcast t.sigwinch
        (Int32.to_int width, Int32.to_int height)
      ; Lwt.return_unit
    | SSH.Set_env {key; value} ->
      Hashtbl.replace t.env key value
      ; Lwt.return_unit
    | SSH.Shell {ic; oc; ec} ->
      let cursor = Lwd.var (0, 0) in
      let sleep ns = Time.sleep_ns (Duration.of_f ns) in
      let now () = Ptime.v (Pclock.now_d_ps ()) in
      let Kit.Engine.{status; window; command; message; quit}, engine =
        Kit.Engine.make ~now ~sleep () in
      let mode = Lwd.var `Normal in
      let tabs = Lwd.var Kit.Tabs.empty in
      let ui =
        let ( let* ) x f = Lwd.bind ~f x in
        let* prompt =
          Kit.Prompt.make ~command ~message cursor status mode window in
        let* window = Kit.Window.make mode window in
        let* tabs = Kit.Tabs.make tabs in
        Lwd.return (Nottui.Ui.vcat [tabs; window; prompt]) in
      Lwt.join
        [
          Nottui'.run ~cursor ~quit (t.size, t.sigwinch) ui ic oc
        ; Kit.Engine.process engine
        ]
      >>= fun () ->
      Logs.debug (fun m -> m "Stop the connection.")
      ; Lwt_switch.turn_off stop
    | SSH.Channel {command; _} ->
      Logs.warn (fun m -> m "Ignore the channel for %S" command)
      ; Lwt.return_unit

  let tcp_handler pk db flow =
    let stop = Lwt_switch.create () in
    let server, msgs = Awa.Server.make pk db in
    let state =
      {env= Hashtbl.create 0x10; sigwinch= Lwt_condition.create (); size= 0, 0}
    in
    let* _t =
      SSH.spawn_server ~stop server msgs flow (ssh_handler ~stop state) in
    Stack.TCP.close flow

  let already_has_password =
    List.exists (function `Password _ -> true | _ -> false)

  let generate_db () =
    let rec go acc = function
      | [] ->
        let module Map = Map.Make (String) in
        let m =
          List.fold_left
            (fun m (user, auth) ->
              match Map.find_opt user m, auth with
              | Some auths, `Password p' when already_has_password auths ->
                Logs.warn (fun m ->
                    m
                      "A password is already defined for the user %S (the new \
                       one is ignored)."
                      user)
                ; m
              | Some auths, `Password p' ->
                Map.add user (`Password p' :: auths) m
              | Some auths, `Key k -> Map.add user (`Key k :: auths) m
              | None, auth -> Map.add user [auth] m)
            Map.empty acc in
        Map.fold
          (fun user auths acc ->
            let keys, ps =
              List.partition_map
                (function
                  | `Key key -> Either.Left key | `Password p -> Either.Right p)
                auths in
            let password = List.nth_opt ps 0 in
            Logs.debug (fun m ->
                m "Add the user %s with %d key(s)." user (List.length keys))
            ; Awa.Auth.make_user user ?password keys :: acc)
          m []
      | x :: r -> (
        match String.split_on_char ':' x with
        | user :: "password" :: password ->
          let password = String.concat ":" password in
          go ((user, `Password password) :: acc) r
        | user :: key -> (
          let key = String.concat ":" key in
          match Awa.Keys.authenticator_of_string key with
          | Ok (`Key key) -> go ((user, `Key key) :: acc) r
          | Ok `No_authentication | Ok (`Fingerprint _) | Error _ ->
            Logs.warn (fun m -> m "Invalid SSH key for the user %S" user)
            ; go acc r)
        | _ ->
          Logs.warn (fun m -> m "Invalid --user argument: %S" x)
          ; go acc r) in
    go [] (Key_gen.users ())

  let start _random _time _mclock stackv4v6 =
    let pk =
      Rresult.R.failwith_error_msg
      @@ Awa.Keys.of_string (Key_gen.private_key ()) in
    let db = generate_db () in
    Stack.TCP.listen ~port:(Key_gen.port ()) (Stack.tcp stackv4v6)
      (tcp_handler pk db)
    ; Stack.listen stackv4v6
end
