type t

val pp : t Fmt.t
val make : ?name:string -> Address.t -> t
val address : t -> Address.t
val equal : t -> t -> bool
val compare : t -> t -> int
val name : t -> string

module Set : Set.S with type elt = t
module Map : Map.S with type key = string

val to_map : Set.t -> t Map.t
