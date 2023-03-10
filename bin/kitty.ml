let () = Printexc.record_backtrace true

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  {Logs.report}

let () =
  let oc = open_out "log.txt" in
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
  ; Logs.set_reporter (reporter (Format.formatter_of_out_channel oc))
  ; Logs.set_level ~all:true (Some Logs.Debug)
  ; Stdlib.at_exit (fun () -> close_out oc)

let now () = Ptime_clock.now ()

let ui, cursor, quit, engine =
  let ( let* ) x f = Lwd.bind ~f x in
  let cursor = Lwd.var (0, 0) in
  let sleep = Lwt_unix.sleep in
  let Kit.Engine.{status; window; command; message; quit}, engine =
    Kit.Engine.make ~now ~sleep () in
  let mode = Lwd.var `Normal in
  let tabs = Lwd.var Kit.Tabs.empty in

  let ui =
    let* prompt = Kit.Prompt.make ~command ~message cursor status mode window in
    let* window = Kit.Window.make mode window in
    let* tabs = Kit.Tabs.make tabs in
    Lwd.return (Nottui.Ui.vcat [tabs; window; prompt]) in
  ui, cursor, quit, engine

let banner engine =
  let open Lwt.Syntax in
  let* () = Kit.Engine.Windows.push_on_console engine "KitOS (c) robur.coop" in
  let* () = Lwt_unix.sleep Duration.(to_f (of_ms 1000)) in
  let* () = Kit.Engine.Windows.push_on_console engine "Hello World!" in
  Lwt.return_unit

let fiber () =
  Lwt.join
    [Nottui_lwt.run ~cursor ~quit ui; Kit.Engine.process engine; banner engine]

let () =
  Lwt_main.run
  @@ Lwt.catch fiber
  @@ fun exn ->
  Logs.err (fun m -> m "Got an exception: %s" (Printexc.to_string exn))
  ; Lwt.return_unit
