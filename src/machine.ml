type action = |

module type S = sig
  type t

  val react : t -> ?prefix:Cri.Protocol.prefix -> Cri.protocol.message -> t * action list
  val write : t -> Cri.Protocol.message list
end
