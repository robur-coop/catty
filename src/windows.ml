type 'c elt = Notty.A.color Art.t * ('c, Message.t) Rb.t

type t =
  { current : Rb.ro elt Lwd.var
  ; mutable current_index : int
  ; windows : < rd : unit ; wr : unit > elt array
  ; now : unit -> Ptime.t
  }

let make ~now =
  let window = Rb.make 0x1000 in
  let nicknames = Art.make () in
  { current = Lwd.var (nicknames, Rb.to_ro window)
  ; current_index = 0
  ; windows = [| (nicknames, window) |]
  ; now
  }

let push_on_console ?(prefix = "kit") t message =
  let nicknames, w = t.windows.(0) in
  Rb.fit_and_push w { time = t.now (); nickname = Art.key prefix; message };
  if snd (Lwd.peek t.current) == Rb.to_ro w then
    Lwd.set t.current (nicknames, Rb.to_ro w);
  Lwt.return_unit

let push t ~nickname message =
  let nicknames, w = t.windows.(t.current_index) in
  Rb.fit_and_push w { Message.time = t.now (); nickname; message };
  Lwd.set t.current (nicknames, Rb.to_ro w);
  Lwt.return_unit
