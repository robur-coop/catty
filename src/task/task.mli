type msg =
  | Message :
      { prefix : Cri.Protocol.prefix option; message : Cri.Protocol.message }
      -> msg

type 'a action = [ `Stop of msg list | `Continue of msg list * 'a ]

module type S = sig
  type t

  val recv :
    t -> ?prefix:Cri.Protocol.prefix -> Cri.Protocol.message -> t action Lwt.t

  val stop : t -> msg list Lwt.t
end

type state
type 'a witness
type value = Value : 'a * (module S with type t = 'a) * ('a -> state) -> value

val inj : (module S with type t = 'a) -> 'a witness
val prj : state -> value
val v : 'a witness -> 'a -> state
val prove : 'a witness -> state -> 'a option

module Ping : sig
  include S

  val v : ?prefix:Cri.Protocol.prefix -> unit -> (t * msg list) Lwt.t
end

val ping : Ping.t witness
