
type pos

val get_x : pos -> int
val get_y : pos -> int

val inc_x : pos -> pos
val inc_y : pos -> pos

val dec_x : pos -> pos
val dec_y : pos -> pos

val validP : pos -> bool

val knownP : pos -> bool

val make_pos : int -> int -> pos

val make_unknown : unit -> pos
