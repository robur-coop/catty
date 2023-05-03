type t = int

let gen =
  let v = ref 0 in
  fun () ->
    incr v;
    !v

let pp = Fmt.int
let console = 0
let equal = Int.equal
let to_int x = x
