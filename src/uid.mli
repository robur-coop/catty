type t

val gen : unit -> t
val pp : t Fmt.t
val console : t
val equal : t -> t -> bool
val to_int : t -> int
