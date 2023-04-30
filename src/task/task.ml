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

module Implicit = Implicit.Make (struct
  type 'a t = (module S with type t = 'a)
end)

type state = Implicit.t = private ..
type 'a witness = 'a Implicit.witness

type value = Implicit.value =
  | Value : 'a * (module S with type t = 'a) * ('a -> state) -> value

let inj = Implicit.inj
let prj = Implicit.prj
let v = Implicit.v
let prove = Implicit.prove

module Ping = struct
  let src = Logs.Src.create "kit.ping"

  module Log = (val Logs.src_log src : Logs.LOG)

  type t = Cri.Protocol.prefix option

  let recv t ?prefix:_ (Cri.Protocol.Message (cmd, v)) =
    match (cmd, v) with
    | Cri.Protocol.Ping, (srv1, srv2) ->
        Log.debug (fun m -> m "Received a ping, respond by a pong.");
        Lwt.return
          (`Continue
            ( [ Message
                  { prefix = t
                  ; message = Message (Cri.Protocol.Pong, (srv1, srv2))
                  }
              ]
            , t ))
    | _ -> Lwt.return (`Continue ([], t))

  let stop _ = Lwt.return []
  let v ?prefix () = Lwt.return (prefix, [])
end

let ping = Implicit.inj (module Ping)
