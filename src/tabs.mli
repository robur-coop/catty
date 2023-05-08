type t

val empty : t
val make : move:(int -> t -> unit) -> t Lwd.var -> Nottui.Ui.t Lwd.t
val v : ?active:int -> [ `Console | `Window of string ] list -> t
val pp : t Fmt.t
