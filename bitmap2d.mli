(** bitmap2d.mli
    interface-file of bitmap2d.ml

    Two-dimensional bitmaps based on the data type Bitmap.

    All indices of rows/columns in 2d-bitmaps are 0-based. (just like arrays)

    Valid bitmap sizes range from 1 to max_bitmap_size.
    2d-Bitmaps are rectangular, not necessarily squared.

    Valid indices for columns in a 2d-bitmap [bm] range from 
    0 to [(Bitmap2d.sizeX bm - 1)].

    Valid indices for rows in a 2d-bitmap [bm] range from 
    0 to [(Bitmap2d.sizeY bm - 1)].

    Most functions in this module that perform side-effects return unit. 

    Most binary operations over bitmaps require that the bitmaps used
    have equal sizes. 

    Unsafe functions do not perform any checks. These functions should
    be used with caution. 

    Status: 
	[1] Sat Aug 31 08:47:58 CEST 2002:
		- created

 **)

open Bitmap

type bitmap2d

exception Bad_BitmapSize
exception Bad_Index


(** Bitmap operations. *)

val max_bitmap_size : int
(** [Bitmap2d.max_bitmap_size] is the maximal # of rows/columns in a bitmap. *)

val sizeX : bitmap2d -> int
val sizeY : bitmap2d -> int
(** [Bitmap2d.sizeY bm] returns the number of rows in bitmap [bm],
    [Bitmap2d.sizeX bm] returns the number of columns. *) 

val valid_sizeP : int -> int -> bool
(** [Bitmap2d.valid_sizeP x y] returns true if [x] and [y] are valid 
    bitmap-size, false otherwise. *)

val make_empty : int -> int -> bitmap2d
(** [Bitmap2d.make_empty x y] returns a fresh bitmap with 
    [y] rows holding [x] bits each.
    All bits of the new bitmap are initialized to 0.
    Raises [Bad_Bitmap_Size] if [(not (valid_sizeP x && valid_sizeP y))]. *)

val make_full : int -> int -> bitmap2d
(** [Bitmap2d.make_full x y] returns a fresh bitmap with 
    [y] rows holding [x] bits each.
    All bits of the new bitmap are initialized to 1.
    Raises [Bad_Bitmap_Size] if [(not (valid_sizeP x && valid_sizeP y))]. *)

val copy : bitmap2d -> bitmap2d
(** [Bitmap2d.copy a] returns a copy of [a], that is, a fresh bitmap
    containing the same elements as [a]. *)

val clear : bitmap2d -> unit


val fetch_row'  : bitmap2d -> int -> bitmap -> bitmap


val blit_row  : bitmap -> int -> bitmap2d -> bitmap2d
val blit_row' : bitmap -> int -> bitmap2d -> bitmap2d
(** [Bitmap2d.blit_row bm y dst] blits the contents of bitmap [bm] into 
    the [y]-th row of 2d-bitmap [dst] and returns [dst].
    Raises [Bad_Bitmap_Size] if [bm] and [dst] have incompatible sizes. *)

val emptyP : bitmap2d -> bool
(** [Bitmap2d.emptyP x] returns true if no bit is set in bitmap [x],
    false otherwise. *)

val fullP : bitmap2d -> bool
(** [Bitmap2d.fullP x] returns true if no bit is clear in bitmap [x],
    false otherwise. *)

val equalP : bitmap2d -> bitmap2d -> bool
(** [Bitmap2d.equalP x y] returns true, if the bitmaps [x] and [y] have 
    the same length and are element-wise equal, false otherwise. *)

val weight : bitmap2d -> int
(** [Bitmap2d.weight bm] returns the number of set bits in [bm]. *)

val bitsetP  : bitmap2d -> int -> int -> bool
val bitsetP' : bitmap2d -> int -> int -> bool
(** [Bitmap2d.bitsetP bm x y] returns true if the bit in the [x]-th column
    and the [y]-row of bitmap [bm] is set, false otherwise.  
    Raises [Bad_Index] if [x] or [y] is out of range. *)

val setbit  : bitmap2d -> int -> int -> unit
val setbit' : bitmap2d -> int -> int -> unit
(** [Bitmap2d.setbit bm x y] destructively sets the bit in the [x]-th column
    and the [y]-row of bitmap [bm].
    Raises [Bad_Index] if [x] or [y] is out of range. *)

val clearbit  : bitmap2d -> int -> int -> unit
val clearbit' : bitmap2d -> int -> int -> unit
(** [Bitmap2d.clearbit bm x y] destructively clears the bit in the [x]-th
    column and the [y]-row of bitmap [bm].
    Raises [Bad_Index] if [x] or [y] is out of range. *)

val to_string : bitmap2d -> bytes
(** [Bitmap2d.to_string bm] returns the textual representation of 
    bitmap [bm]. *)

val row_to_string' : bitmap2d -> int -> bytes
(** [Bitmap2d.row_to_string' bm y] returns the textual representation
    of the [y]-th row of bitmap [bm]. *)