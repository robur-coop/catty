module type KEY_INFO = sig
  type 'a t
end

module Make (Key_info : KEY_INFO) : sig
  type t = private ..
  type 'a key = 'a Key_info.t
  type 'a inj = 'a -> t

  module type WITNESS = sig
    type a
    type t += T of a

    val key : a key
    val inj : a inj
  end

  type 'a witness = (module WITNESS with type a = 'a)
  type pack = Key : 'a key -> pack
  type value = Value : 'a * 'a key * 'a inj -> value

  val inj : 'a key -> 'a witness
  val prj : t -> value
  val bindings : unit -> pack list
  val v : 'a witness -> 'a -> t
  val prove : 'a witness -> t -> 'a option
  val equal : 'a witness -> t -> t -> bool
end
