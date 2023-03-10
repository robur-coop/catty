type 'c elt = Notty.A.color Art.t * ('c, Message.t) Rb.t

type t = {
    current: Rb.ro elt Lwd.var
  ; windows: < rd: unit ; wr: unit > elt array
  ; now: unit -> Ptime.t
}

let make ~now =
  let window = Rb.make 0x1000 in
  let nicknames = Art.make () in
  Art.insert nicknames (Art.key "root") Notty.A.white
  ; {
      current= Lwd.var (nicknames, Rb.to_ro window)
    ; windows= [|nicknames, window|]
    ; now
    }

let root = Art.key "root"

let push_on_console t message =
  let nicknames, w = t.windows.(0) in
  Rb.fit_and_push w {time= t.now (); nickname= root; message}
  ; if snd (Lwd.peek t.current) == Rb.to_ro w then
      Lwd.set t.current (nicknames, Rb.to_ro w)
  ; Lwt.return_unit
