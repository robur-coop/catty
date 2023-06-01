type t

val make : int -> t
val fill : t -> Rp.Cursor.cursor -> unit
val move_backward : t -> Rp.Cursor.cursor -> Rp.Cursor.cursor
val move_forward : t -> Rp.Cursor.cursor -> Rp.Cursor.cursor
