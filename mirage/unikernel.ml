type state =
  { env : (string, string) Hashtbl.t
  ; sigwinch : (int * int) Lwt_condition.t
  ; mutable size : int * int
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
    (Stack : Tcpip.Stack.V4V6)
    (Happy_eyeballs : Mimic_happy_eyeballs.S with type flow = Stack.TCP.flow) =
struct
  module SSH = Awa_mirage.Make (Stack.TCP) (Time) (Mclock)
  module Nottui' = Nottui_mirage.Make (Time)
  module NSS = Ca_certs_nss.Make (Pclock)

  module TLS = struct
    include Tls_mirage.Make (Stack.TCP)

    type endpoint = Tls.Config.client * Catty.Address.t * Happy_eyeballs.t

    let connect (tls, addr, he) =
      let port =
        match addr with `Domain (_, port) | `Inet (_, port) -> port
      in
      let addr, host =
        match addr with
        | `Domain (domain, _) -> (Domain_name.to_string domain, Some domain)
        | `Inet (ipaddr, _) -> (Ipaddr.to_string ipaddr, None)
      in
      Happy_eyeballs.resolve he addr [ port ] >>= function
      | Ok (_, flow) -> client_of_flow tls ?host flow
      | Error (`Msg _err) ->
          (* TODO(dinosaure): emit the error *) Lwt.return_error `Closed
  end

  module TCP = struct
    include Stack.TCP

    type endpoint = Catty.Address.t * Happy_eyeballs.t

    let connect (addr, he) =
      let port =
        match addr with `Domain (_, port) | `Inet (_, port) -> port
      in
      let addr, host =
        match addr with
        | `Domain (domain, _) -> (Domain_name.to_string domain, Some domain)
        | `Inet (ipaddr, _) -> (Ipaddr.to_string ipaddr, None)
      in
      Happy_eyeballs.resolve he addr [ port ] >>= function
      | Ok (_, flow) -> Lwt.return_ok flow
      | Error (`Msg _err) ->
          (* TODO(dinosaure): emit the error *) Lwt.return_error `Closed
  end

  let tcp_edn, tcp_protocol = Mimic.register ~name:"tcp/ip" (module TCP)

  let tls_edn, tls_protocol =
    Mimic.register ~priority:10 ~name:"tls" (module TLS)

  let ssh_handler ~host ~stop t ctx = function
    | SSH.Pty_req { width; height; max_width; max_height; term } ->
        t.size <- (Int32.to_int width, Int32.to_int height);
        Lwt.return_unit
    | SSH.Pty_set { width; height; max_width; max_height } ->
        Lwt_condition.broadcast t.sigwinch
          (Int32.to_int width, Int32.to_int height);
        Lwt.return_unit
    | SSH.Set_env { key; value } ->
        Hashtbl.replace t.env key value;
        Lwt.return_unit
    | SSH.Shell { ic; oc; ec } ->
        let ic () =
          ic () >|= function
          | `Data cs ->
              `Data (Cstruct.map (function '\r' -> '\n' | chr -> chr) cs)
          | `Eof -> `Eof
        in
        let ui, cursor, quit, engine, action =
          let ( let* ) x f = Lwd.bind ~f x in
          let cursor = Lwd.var (0, 0)
          and mode = Lwd.var `Normal
          and status = Lwd.var `None in
          let now () = Ptime.v (Pclock.now_d_ps ()) in
          let user =
            Catty.User.make ~realname:(Key_gen.realname ())
              (List.map Cri.Nickname.of_string_exn (Key_gen.nicknames ()))
          in
          let (quit, command, message, action), engine =
            Catty.Engine.make ~ctx ~now
              ~sleep:(fun ns -> Time.sleep_ns (snd (Pclock.now_d_ps ())))
              ~host user
          in
          let windows = Catty.Windows.make ~now host in

          let rec go () =
            Lwt_stream.get action >>= function
            | None -> Lwt.return_unit
            | Some (Catty.Action.Set_status v) ->
                Lwd.set status v;
                go ()
            | Some (Catty.Action.New_window (uid, name)) ->
                Catty.Windows.new_window windows ~uid ~name >>= go
            | Some (Catty.Action.Delete_window uid) ->
                Catty.Windows.delete_window windows ~uid >>= go
            | Some (Catty.Action.New_message (uid, msg)) ->
                Catty.Windows.push_on windows ~uid msg >>= go
          in

          let ui =
            let* prompt =
              Catty.Prompt.make ~command ~message cursor status mode
                (Catty.Windows.var windows)
            in
            let* window = Catty.Windows.Ui.make mode windows in
            Lwd.return (Nottui.Ui.vcat [ window; prompt ])
          in
          (ui, cursor, quit, engine, go)
        in
        Lwt.join
          [ Nottui'.run ~cursor ~quit (t.size, t.sigwinch) ui ic oc
          ; Catty.Engine.process engine
          ; action ()
          ]
        >>= fun () -> Lwt_switch.turn_off stop
    | SSH.Channel { cmd; _ } ->
        Logs.warn (fun m -> m "Ignore the channel for %S" cmd);
        Lwt.return_unit

  let tcp_handler ~host pk db ctx flow =
    let stop = Lwt_switch.create () in
    let server, msgs = Awa.Server.make pk db in
    let state =
      { env = Hashtbl.create 0x10
      ; sigwinch = Lwt_condition.create ()
      ; size = (0, 0)
      }
    in
    let* _t =
      SSH.spawn_server ~stop server msgs flow
        (ssh_handler ~host ~stop state ctx)
    in
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
                match (Map.find_opt user m, auth) with
                | Some auths, `Password p' when already_has_password auths ->
                    Logs.warn (fun m ->
                        m
                          "A password is already defined for the user %S (the \
                           new one is ignored)."
                          user);
                    m
                | Some auths, `Password p' ->
                    Map.add user (`Password p' :: auths) m
                | Some auths, `Key k -> Map.add user (`Key k :: auths) m
                | None, auth -> Map.add user [ auth ] m)
              Map.empty acc
          in
          Map.fold
            (fun user auths acc ->
              let keys, ps =
                List.partition_map
                  (function
                    | `Key key -> Either.Left key
                    | `Password p -> Either.Right p)
                  auths
              in
              let password = List.nth_opt ps 0 in
              Logs.debug (fun m ->
                  m "Add the user %s with %d key(s)." user (List.length keys));
              Awa.Auth.make_user user ?password keys :: acc)
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
                  Logs.warn (fun m -> m "Invalid SSH key for the user %S" user);
                  go acc r)
          | _ ->
              Logs.warn (fun m -> m "Invalid --user argument: %S" x);
              go acc r)
    in
    go [] (Key_gen.users ())

  let resolve ctx =
    let authenticator =
      match NSS.authenticator () with
      | Ok v -> v
      | Error (`Msg err) -> failwith err
    in

    let k0 he dst with_tls =
      match with_tls with
      | true -> Lwt.return_some (Tls.Config.client ~authenticator (), dst, he)
      | false -> Lwt.return_none
    in
    let k1 he dst with_tls =
      match with_tls with
      | true -> Lwt.return_none
      | false -> Lwt.return_some (dst, he)
    in
    ctx
    |> Mimic.fold ~k:k0 tls_edn
         Mimic.Fun.
           [ req Happy_eyeballs.happy_eyeballs
           ; req Catty.Engine.Connect.dst
           ; req Catty.Engine.Connect.tls
           ]
    |> Mimic.fold ~k:k1 tcp_edn
         Mimic.Fun.
           [ req Happy_eyeballs.happy_eyeballs
           ; req Catty.Engine.Connect.dst
           ; req Catty.Engine.Connect.tls
           ]

  let start _random _time _mclock stackv4v6 ctx =
    let ctx = resolve ctx in
    let pk =
      Rresult.R.failwith_error_msg
      @@ Awa.Keys.of_string (Key_gen.private_key ())
    in
    let db = generate_db () in
    let host =
      Rresult.R.failwith_error_msg (Domain_name.of_string (Key_gen.host ()))
    in
    Stack.TCP.listen ~port:(Key_gen.port ()) (Stack.tcp stackv4v6)
      (tcp_handler ~host pk db ctx);
    Stack.listen stackv4v6
end
