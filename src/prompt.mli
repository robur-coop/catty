(** The Kit's prompt.

    The Kit's prompt displays 3 lines:
    - the message's prompt (where the user writes what he/she wants to send)
    - the command's prompt (where the user can execute some commands - like [connect])
    - the status
*)

type cursor = int * int

val make :
     command:(string -> unit)
  -> message:(string -> unit)
  -> cursor Lwd.var
  -> Status.t Lwd.var
  -> Mode.t Lwd.var
  -> Rb.ro Windows.elt Lwd.var
  -> Nottui.Ui.t Lwd.t
