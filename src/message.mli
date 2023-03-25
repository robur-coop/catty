type t = { nickname : Art.key; message : string; time : Ptime.t }

val pp : t Fmt.t
val make : nickname:Art.key -> time:Ptime.t -> string -> t
