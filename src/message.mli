type t

val pp : t Fmt.t
val split_at : len:int -> t -> Notty.image list
val make : nickname:Art.key -> time:Ptime.t -> string list -> t
val nickname : t -> Art.key
val time : t -> Ptime.t

val msgf :
     now:(unit -> Ptime.t)
  -> ?prefix:Cri.Protocol.prefix
  -> ?server:Server.t
  -> ('a, Format.formatter, unit, t) format4
  -> 'a

(** / *)

val render_time : Ptime.t -> Notty.I.t
val width_time : int
