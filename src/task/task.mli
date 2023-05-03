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
  val name : string
end

type state
type 'a witness
type value = Value : 'a * (module S with type t = 'a) * ('a -> state) -> value

val inj : (module S with type t = 'a) -> 'a witness
val prj : state -> value
val v : 'a witness -> 'a -> state
val prove : 'a witness -> state -> 'a option

(** Syntax *)

type 'a t = ('a * msg list) Lwt.t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
val return : 'a -> 'a t

(** A simple ping state. *)

val ping : ?prefix:Cri.Protocol.prefix -> unit -> state t
