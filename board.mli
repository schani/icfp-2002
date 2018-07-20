
open Bitmap

type board

type cell =
  | C_Plain
  | C_Water
  | C_Wall
  | C_Homebase

val make_empty' : int -> int -> board
(** [Board.make_empty x y] returns a fresh board with [x+2] columns and
    [y+2] rows. All cells in the board are initially set to plain 
    (".", a dot), except for all cells on the edge of the board.
    These cells are initialized to "#" (a wall). *)

val copy : board -> board
(** [Board.copy b] returns a copy of [b], that is, a fresh board
    containing the same elements as [b]. *)

val sizeX : board -> int
val sizeY : board -> int

val rawsizeX : board -> int
val rawsizeY : board -> int

val equalP : board -> board -> bool
(** [Board.equalP bm1 bm2] returns true, if the boards [bm1] and [bm2]
    are identical, false otherwise. *)

val getcell' : board -> int -> int -> cell
(** [Board.getcell b x y] returns the cell in the [x]-th column and
    the [y]-th row of the board [b]. *)
    
val setcell' : board -> int -> int -> cell -> unit
(** [Board.setcell b x y c] destructively sets the cell in [x]-th column
    and the [y]-th row of board [b] to [c] and returns unit. *)

val cannot_visit_cell' : board -> int -> int -> bool
(** [Board.cannot_visit_cell b x y] returns true, if the cell in the
    [x]-th column and the [y]-th row of board [b] can not be visited 
    (the cell represents water or a wall). The function returns false
    otherwise. *)

(* val nr_lethal_fields : board -> int *)
(** [Board.nr_lethal_cell b] returns the number of lethal cells 
    (cells containing water) on board [b]. *)

val row_from_string : string -> bitmap
val row_from_string' : int -> string -> bitmap

val row_to_string : bitmap -> bytes

val row_to_string' : bitmap -> bytes

val blit_row' : bitmap -> int -> board -> board

val fetch_row' : board -> int -> bitmap -> bitmap

