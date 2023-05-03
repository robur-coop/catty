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
  let name = "ping"
end

type 'a t = ('a * msg list) Lwt.t

let ( let* ) x f =
  let open Lwt.Infix in
  x >>= fun (v, msgs0) ->
  f v >|= fun (v, msgs1) -> (v, List.rev_append msgs0 msgs1)

let ( and* ) a b =
  let open Lwt.Infix in
  Lwt.both a b >>= fun ((a, msgs0), (b, msgs1)) ->
  Lwt.return ((a, b), List.rev_append msgs0 msgs1)

let return v = Lwt.return (v, [])
let ping_witness = Implicit.inj (module Ping)

let ping ?prefix () =
  let open Lwt.Syntax in
  let* state, msgs = Ping.v ?prefix () in
  Lwt.return (Implicit.v ping_witness state, msgs)
