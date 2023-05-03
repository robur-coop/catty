type t = [ `Domain of [ `host ] Domain_name.t * int | `Inet of Ipaddr.t * int ]

let pp ppf = function
  | `Domain (host, port) -> Fmt.pf ppf "%a:%d" Domain_name.pp host port
  | `Inet (ipaddr, port) -> Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port

let equal a b =
  match (a, b) with
  | `Inet (ipaddr, p), `Inet (ipaddr', p') ->
      Ipaddr.compare ipaddr ipaddr' = 0 && p = p'
  | `Domain (domain, p), `Domain (domain', p') ->
      Domain_name.equal domain domain' && p = p'
  | _ -> false

let to_string ?(without_default = false) t =
  match (t, without_default) with
  | `Domain (hostname, 6697), true -> Domain_name.to_string hostname
  | `Inet (ipaddr, 6697), true -> Ipaddr.to_string ipaddr
  | t, _ -> Fmt.to_to_string pp t

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let of_string ?port:(default = 6697) str =
  let[@warning "-8"] (port :: rest) =
    match List.rev (String.split_on_char ':' str) with
    | [ host ] -> [ Int.to_string default; host ]
    | _ :: _ as result -> result
    | [] -> assert false
  in
  let host = String.concat ":" (List.rev rest) in
  match
    ( Result.bind (Domain_name.of_string host) Domain_name.host
    , Ipaddr.with_port_of_string ~default (host ^ ":" ^ port)
    , int_of_string_opt port )
  with
  | Ok host, _, Some port -> Ok (`Domain (host, port))
  | Error _, Ok (ipaddr, port), _ -> Ok (`Inet (ipaddr, port))
  | _ -> error_msgf "Invalid destination: %S" str
