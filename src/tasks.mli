(** Implementation of some useful tasks. *)

val nickname :
     now:(unit -> Ptime.t)
  -> action:(Action.t -> unit)
  -> ?prefix:Cri.Protocol.prefix
  -> Server.t
  -> Cri.Nickname.t list
  -> (Task.state * Task.msg list) Lwt.t

val error :
     now:(unit -> Ptime.t)
  -> action:(Action.t -> unit)
  -> Server.t
  -> (Task.state * Task.msg list) Lwt.t

val notice :
     now:(unit -> Ptime.t)
  -> action:(Action.t -> unit)
  -> Server.t
  -> (Task.state * Task.msg list) Lwt.t

(** Extended API for tasks. *)

val current_nickname : Task.state list -> Cri.Nickname.t
