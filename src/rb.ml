type ('c, 'a) t = {
    arr: 'a option array
  ; mutable rd_cursor: int
  ; mutable wr_cursor: int
}
  constraint 'c = < .. >

type ro = < rd: unit >
type wo = < wr: unit >
type 'a rd = < rd: unit ; .. > as 'a
type 'a wr = < wr: unit ; .. > as 'a

let to_power_of_two v =
  let res = ref (pred v) in
  res := !res lor (!res lsr 1)
  ; res := !res lor (!res lsr 2)
  ; res := !res lor (!res lsr 4)
  ; res := !res lor (!res lsr 8)
  ; res := !res lor (!res lsr 16)
  ; succ !res

let make len =
  {arr= Array.make (to_power_of_two len) None; rd_cursor= 0; wr_cursor= 0}

exception Full
exception Empty

let length t = t.wr_cursor - t.rd_cursor
let is_empty t = t.rd_cursor = t.wr_cursor
let available t = Array.length t.arr - (t.wr_cursor - t.rd_cursor)
let is_full t = length t = Array.length t.arr
let mask t v = v land (Array.length t.arr - 1)

let push t v =
  if is_full t then raise Full
  ; t.arr.(mask t t.wr_cursor) <- Some v
  ; t.wr_cursor <- t.wr_cursor + 1

let pop t =
  if is_empty t then raise Empty
  ; let[@warning "-8"] (Some v) = t.arr.(mask t t.rd_cursor) in
    t.rd_cursor <- t.rd_cursor + 1
    ; v

let fit_and_push t v =
  if is_full t then ignore (pop t)
  ; push t v

let iter ~f t a =
  let i = ref t.rd_cursor in
  let a = ref a in
  while !i <> t.wr_cursor do
    a := f (Option.get t.arr.(mask t !i)) !a
    ; incr i
  done
  ; !a

let rev_iter ~f t a =
  let i = ref (t.wr_cursor - 1) in
  let a = ref a in
  while !i >= t.rd_cursor do
    a := f (Option.get t.arr.(mask t !i)) !a
    ; decr i
  done
  ; !a

let ( .%[] ) t idx =
  if idx >= length t then invalid_arg "Out of bounds"
  ; Option.get t.arr.(mask t (t.rd_cursor + idx))

external to_ro : ('c rd, 'a) t -> (ro, 'a) t = "%identity"
external to_wo : ('c wr, 'a) t -> (wo, 'a) t = "%identity"
