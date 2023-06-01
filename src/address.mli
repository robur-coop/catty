type t = [ `Domain of [ `host ] Domain_name.t * int | `Inet of Ipaddr.t * int ]

val pp : t Fmt.t
val equal : t -> t -> bool
val to_string : ?without_default:bool -> t -> string
val of_string : ?port:int -> string -> (t, [> `Msg of string ]) result
