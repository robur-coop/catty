let src = Logs.Src.create "kit.windows"

module Log = (val Logs.src_log src : Logs.LOG)

[@@@warning "-32"]

type 'c elt = 'c Window.elt =
  { nicknames : Notty.A.color Art.t
  ; buffer : ('c, Message.t) Rb.t
  ; name : Window.Name.t
  ; uid : Uid.t
  }

let pp_elt ppf { name; _ } = Window.Name.pp ppf name

module Zip : sig
  type 'a t

  val move_backward : 'a t -> 'a t option
  val move_forward : 'a t -> 'a t option
  val move_to : 'a t -> int -> 'a t option
  val insert : 'a -> 'a t -> 'a t
  val delete : 'a t -> ('a * 'a t) option
  val singleton : 'a -> 'a t
  val get : 'a t -> 'a option
  val find : ('a -> bool) -> 'a t -> 'a
  val to_index : 'a t -> int
  val fold : (active:bool -> 'acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val pp : 'a Fmt.t -> 'a t Fmt.t
end = struct
  type 'a t = 'a list * 'a list

  let to_index (p, _) = List.length p
  let move_backward = function [], _ -> None | a :: p, l -> Some (p, a :: l)

  let move_forward = function
    | _, [] | _, [ _ ] -> None
    | p, a :: l -> Some (a :: p, l)

  let move_to t idx =
    if idx < 0 then
      List.fold_left
        (fun acc () -> Option.bind acc move_backward)
        (Some t)
        (List.init (abs idx) (Fun.const ()))
    else
      List.fold_left
        (fun acc () -> Option.bind acc move_forward)
        (Some t)
        (List.init idx (Fun.const ()))

  let insert v (p, l) =
    match l with
    | current :: rest -> (p, current :: v :: rest)
    | [] -> (p, [ v ])

  let get (_, l) = match l with x :: _ -> Some x | [] -> None
  let delete = function _, [] -> None | p, a :: l -> Some (a, (p, l))
  let empty = ([], [])
  let singleton v = insert v empty

  let fold f acc (p, l) =
    let acc = List.fold_left (f ~active:false) acc p in
    match l with
    | [] -> acc
    | active :: l ->
        let acc = f ~active:true acc active in
        let acc = List.fold_left (f ~active:false) acc l in
        acc

  let find predicate (p, l) =
    try List.find predicate (List.rev p)
    with Not_found -> List.find predicate l

  let pp pp_elt ppf (p, l) =
    Fmt.pf ppf "@[<hov>(%a,@ %a)@]"
      Fmt.(Dump.list pp_elt)
      p
      Fmt.(Dump.list pp_elt)
      l
end

type t =
  { current : Rb.ro elt Lwd.var
  ; tabs : Tabs.t Lwd.var
  ; mutable windows : < rd : unit ; wr : unit > elt Zip.t
  ; now : unit -> Ptime.t
  ; host : [ `raw ] Domain_name.t
  }

let make ~now host =
  let buffer = Rb.make 0x1000 in
  let nicknames = Art.make () in
  let elt = { nicknames; name = Console; buffer; uid = Uid.console } in
  { current = Lwd.var { elt with buffer = Rb.to_ro buffer }
  ; tabs = Lwd.var Tabs.empty
  ; windows = Zip.singleton elt
  ; now
  ; host
  }

let var { current; _ } = current
let tabs { tabs; _ } = tabs

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
  Lwt.return_unit

let push_on_current t msg =
  let[@warning "-8"] (Some { nicknames; name; buffer; uid }) =
    Zip.get t.windows
  in
  Rb.fit_and_push buffer msg;
  Lwd.set t.current { nicknames; name; buffer = Rb.to_ro buffer; uid };
  Lwt.return_unit

let push_on t ~uid msg =
  match Zip.get t.windows with
  | Some { uid = uid'; _ } when Uid.equal uid uid' -> push_on_current t msg
  | _ -> (
      try
        let { buffer; _ } =
          Zip.find (fun { uid = uid'; _ } -> Uid.equal uid uid') t.windows
        in
        Rb.fit_and_push buffer msg;
        Lwt.return_unit
      with Not_found -> Lwt.return_unit)

let zip_to_tabs t =
  let v = ref 0 in
  let f ~active (acc, idx) elt =
    if active then v := idx;
    let v =
      match elt.name with
      | Window.Name.Console -> `Console
      | Window.Name.Name name -> `Window name
    in
    (v :: acc, succ idx)
  in
  let tabs = Zip.fold f ([], 0) t.windows |> fst |> List.rev in
  Tabs.v ~active:!v tabs

let move_forward t =
  match Zip.move_forward t.windows with
  | None -> Lwt.return_unit
  | Some windows ->
      t.windows <- windows;
      Lwd.set t.tabs (zip_to_tabs t);
      let current = Option.get (Zip.get windows) in
      Lwd.set t.current { current with buffer = Rb.to_ro current.buffer };
      Lwt.return_unit

let move_backward t =
  match Zip.move_backward t.windows with
  | None -> Lwt.return_unit
  | Some windows ->
      t.windows <- windows;
      Lwd.set t.tabs (zip_to_tabs t);
      let current = Option.get (Zip.get windows) in
      Lwd.set t.current { current with buffer = Rb.to_ro current.buffer };
      Lwt.return_unit

let move_to t idx =
  Log.debug (fun m -> m "old: %a" (Zip.pp pp_elt) t.windows);
  match Zip.move_to t.windows idx with
  | None -> Fmt.invalid_arg "Invalid given window index: %d" idx
  | Some windows ->
      t.windows <- windows;
      Lwd.set t.tabs (zip_to_tabs t);
      let current = Option.get (Zip.get windows) in
      Lwd.set t.current { current with buffer = Rb.to_ro current.buffer }

let new_window t ~uid ~name =
  let buffer = Rb.make 0x1000 in
  let nicknames = Art.make () in
  let elt = { Window.nicknames; name = Window.Name.Name name; buffer; uid } in
  Lwd.set t.current { elt with buffer = Rb.to_ro elt.buffer };
  t.windows <- Zip.insert elt t.windows;
  t.windows <- Option.get (Zip.move_forward t.windows);
  Lwd.set t.tabs (zip_to_tabs t);
  Lwt.return_unit

module Ui = struct
  let make mode t =
    let ( let* ) x f = Lwd.bind ~f x in
    let* current = Window.make mode t.current in
    let* tabs =
      let move idx _ = try move_to t idx with _ -> () in
      Tabs.make ~move t.tabs
    in
    Lwd.return (Nottui.Ui.vcat [ tabs; current ])
end
