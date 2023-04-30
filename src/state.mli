open Task

module Nickname : sig
  include S

  val current_nickname : t -> Cri.Nickname.t

  val v :
       now:(unit -> Ptime.t)
    -> action:(Action.t -> unit)
    -> ?prefix:Cri.Protocol.prefix
    -> Server.t
    -> Cri.Nickname.t list
    -> (t * msg list) Lwt.t
end

module Error : sig
  include S

  val v :
       now:(unit -> Ptime.t)
    -> action:(Action.t -> unit)
    -> Server.t
    -> (t * msg list) Lwt.t
end

module Notice : sig
  include S

  val v :
       now:(unit -> Ptime.t)
    -> action:(Action.t -> unit)
    -> Server.t
    -> (t * msg list) Lwt.t
end

val nickname : Nickname.t Task.witness
val error : Error.t Task.witness
val notice : Notice.t Task.witness

(** Extended API for tasks. *)

val current_nickname : Task.state list -> Cri.Nickname.t option
