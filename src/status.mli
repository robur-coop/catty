type t =
  [ `Error of string | `Progress of Uchar.t * string | `Done of string | `None ]

val pp : t Fmt.t
val render : t -> Nottui.Ui.t
val errorf : ('a, Format.formatter, unit, t) format4 -> 'a

val loading :
     sleep:(float -> unit Lwt.t)
  -> stop:unit Lwt.t
  -> text:string
  -> (t -> unit)
  -> unit Lwt.t
