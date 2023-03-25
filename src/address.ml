type t = [ `Domain of [ `host ] Domain_name.t * int | `Inet of Ipaddr.t * int ]

let pp ppf = function
  | `Domain (host, port) -> Fmt.pf ppf "%a:%d" Domain_name.pp host port
  | `Inet (ipaddr, port) -> Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port

