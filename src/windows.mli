type t

type 'c elt =
  { nicknames : Notty.A.color Art.t
  ; buffer : ('c, Message.t) Rb.t
  ; name : Window.Name.t
  ; uid : Uid.t
  }

val make : now:(unit -> Ptime.t) -> [ `raw ] Domain_name.t -> t
val var : t -> Rb.ro elt Lwd.var
val tabs : t -> Tabs.t Lwd.var

val new_window : t -> uid:Uid.t -> name:string -> unit Lwt.t
(** [new_window t ~uid ~name] makes a new window with the given [name]. *)

val delete_window : t -> uid:Uid.t -> unit Lwt.t

(** Push a message into a window. *)

val push_on_console : t -> string list -> unit Lwt.t
val push_on_current : t -> Message.t -> unit Lwt.t
val push_on : t -> uid:Uid.t -> Message.t -> unit Lwt.t

(** Move over windows. *)

val move_backward : t -> unit Lwt.t
val move_forward : t -> unit Lwt.t

module Ui : sig
  val make : Mode.t Lwd.var -> t -> Nottui.Ui.t Lwd.t
end
