type t = int

let gen =
  let v = ref (-1) in
  fun () ->
    incr v;
    !v

let pp = Fmt.int
