type server

module Set : Set.S with type elt = server

type t = { quit : unit Lwt.t; servers : Set.t }

type msg = Task.msg =
  | Message :
      { prefix : Cri.Protocol.prefix option; message : Cri.Protocol.message }
      -> msg

val empty : quit:unit Lwt.t -> t
val server : server -> Server.t

val add_server :
     t
  -> stop:Lwt_switch.t
  -> thread:(unit, Cri_lwt.error) result Lwt.t
  -> send:Cri_lwt.send
  -> Server.t
  -> Task.state list
  -> t

val servers : t -> Server.t Server.Map.t
val stop : t -> unit Lwt.t
val send_of_server : on:Server.t -> t -> Cri_lwt.send option
val add_task : on:Server.t -> Task.state * msg list -> t -> t Lwt.t
val tasks_of_server : on:Server.t -> t -> Task.state list
val send_msgs : Cri_lwt.send -> msg list -> unit

val process :
     on:Server.t
  -> Cri.Protocol.prefix option * Cri.Protocol.message
  -> t
  -> t option Lwt.t
