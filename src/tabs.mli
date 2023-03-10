type t

val empty : t
val next : t -> t
val prev : t -> t
val make : t Lwd.var -> Nottui.Ui.t Lwd.t
