type t = { address : Address.t; name : string }

let pp ppf { name; _ } = Fmt.string ppf name

let make ?name address =
  let name =
    match name with
    | Some name -> name
    | None -> Address.to_string ~without_default:true address
  in
  { address; name }

let address { address; _ } = address
let compare a b = String.compare a.name b.name
let equal a b = compare a b = 0

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Map = Map.Make (String)

let to_map servers =
  Set.fold
    (fun ({ name; _ } as server) -> Map.add name server)
    servers Map.empty
