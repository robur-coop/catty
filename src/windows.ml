[@@@warning "-32"]

module Name = struct
  type t = Console | Name of string

  let inf = -1
  and sup = 1

  let compare a b =
    match (a, b) with
    | Console, Console -> 0
    | Name a, Name b -> String.compare a b
    | Console, Name _ -> inf
    | Name _, Console -> sup
end

type 'c elt =
  { nicknames : Notty.A.color Art.t
  ; buffer : ('c, Message.t) Rb.t
  ; name : Name.t
  ; uid : Uid.t
  }

module Zip : sig
  type 'a t

  val move_backward : 'a t -> 'a t option
  val move_forward : 'a t -> 'a t option
  val insert : 'a -> 'a t -> 'a t
  val delete : 'a t -> ('a * 'a t) option
  val singleton : 'a -> 'a t
  val get : 'a t -> 'a option
  val find : ('a -> bool) -> 'a t -> 'a
end = struct
  type 'a t = 'a list * 'a list

  let move_backward = function [], _ -> None | a :: p, l -> Some (p, a :: l)
  let move_forward = function _, [] -> None | p, a :: l -> Some (a :: p, l)
  let insert v (p, l) = (p, v :: l)
  let get (_, l) = match l with x :: _ -> Some x | [] -> None
  let delete = function _, [] -> None | p, a :: l -> Some (a, (p, l))
  let empty = ([], [])
  let singleton v = insert v empty

  let find predicate (p, l) =
    try List.find predicate (List.rev p)
    with Not_found -> List.find predicate l
end

type t =
  { current : Rb.ro elt Lwd.var
  ; windows : < rd : unit ; wr : unit > elt Zip.t
  ; now : unit -> Ptime.t
  ; host : [ `raw ] Domain_name.t
  }

let make ~now host =
  let buffer = Rb.make 0x1000 in
  let nicknames = Art.make () in
  let elt = { nicknames; name = Console; buffer; uid = Uid.console } in
  { current = Lwd.var { elt with buffer = Rb.to_ro buffer }
  ; windows = Zip.singleton elt
  ; now
  ; host
  }

let var { current; _ } = current

let push_on_console t message =
  let { nicknames; name = _; buffer; _ } =
    Zip.find (fun { uid; _ } -> Uid.equal uid Uid.console) t.windows
  in
  let msg =
    Message.make
      ~nickname:(Art.key (Domain_name.to_string t.host))
      ~time:(t.now ()) message
  in
  Rb.fit_and_push buffer msg;
  if Uid.equal (Lwd.peek t.current).uid Uid.console then
    Lwd.set t.current
      { nicknames; name = Console; buffer = Rb.to_ro buffer; uid = Uid.console };
  Lwt.return t

let push_on_current t msg =
  let[@warning "-8"] (Some { nicknames; name; buffer; uid }) =
    Zip.get t.windows
  in
  Rb.fit_and_push buffer msg;
  Lwd.set t.current { nicknames; name; buffer = Rb.to_ro buffer; uid };
  Lwt.return t

let push_on t ~uid msg =
  match Zip.get t.windows with
  | Some { uid = uid'; _ } when Uid.equal uid uid' -> push_on_current t msg
  | _ -> (
      try
        let { buffer; _ } =
          Zip.find (fun { uid = uid'; _ } -> Uid.equal uid uid') t.windows
        in
        Rb.fit_and_push buffer msg;
        Lwt.return t
      with Not_found -> Lwt.return t)

let move_forward t =
  match Zip.move_forward t.windows with
  | None -> t
  | Some windows -> { t with windows }

let move_backward t =
  match Zip.move_backward t.windows with
  | None -> t
  | Some windows -> { t with windows }

let new_window t ~uid ~name =
  let buffer = Rb.make 0x1000 in
  let nicknames = Art.make () in
  let elt = { nicknames; name = Name name; buffer; uid } in
  Lwd.set t.current { elt with buffer = Rb.to_ro elt.buffer };
  { t with windows = Zip.insert elt t.windows }
