(** bitmap.mli
    interface-file of bitmap.ml

    Bitmaps are essentially arrays of bools, with the noticable exception
    that bits can not be shared among different bitmaps.

    All indices of bits in bitmaps are 0-based. (just like arrays)
    Valid bitmap sizes range from 1 to max_bitmap_size.
    Valid indices of bits in a bitmap [x] range 0 to [(Bitmap.size x - 1)].
    Most functions in this module that perform side-effects return unit. 
    Most binary operations over bitmaps require that the bitmaps used
    have equal sizes. 

    Unsafe functions do not perform any checks. These functions should
    be used with caution. 

    Status: 
	[1] Sat Aug 31 08:33:49 CEST 2002: 
		- created
 **)


type bitmap

exception Bad_BitmapSize
exception Bad_Index
exception Bad_String

(** Bitmap operations. *)

val max_bitmap_size : int
(** [Bitmap.max_bitmap_size] is the maximal # of bits in a bitmap. *)

val size : bitmap -> int
(** [Bitmap.size bm] returns the number of bits in bitmap [bm]. *) 

val valid_sizeP : int -> bool
(** [Bitmap.valid_sizeP n] returns true if [n] is a valid bitmap-size,
    false otherwise. *)

val make_empty : int -> bitmap
(** [Bitmap.make_empty n] returns a fresh bitmap with exactly [n] bits.
    All bits of the new bitmap are initialized to 0.
    Raises [Bad_Bitmap_Size] if [(not (valid_sizeP n))]. *)

val make_full : int -> bitmap
(** [Bitmap.make_full n] returns a fresh bitmap with exactly [n] bits.
    All bits of the new bitmap are initialized to 1.
    Raises [Bad_Bitmap_Size] if [(not (valid_sizeP n))]. *)

val copy : bitmap -> bitmap
(** [Bitmap.copy a] returns a copy of [a], that is, a fresh bitmap
    containing the same elements as [a]. *)

val clear : bitmap -> unit 


val blit : bitmap -> bitmap -> bitmap
(** [Bitmap.blit src dst] blits the contents of bitmap [src] into 
    bitmap [dst] and returns [dst].
    Raises [Bad_Bitmap_Size] if [src] and [dst] do not have the same size. *)

val emptyP : bitmap -> bool
(** [Bitmap.emptyP x] returns true if no bit is set in bitmap [x],
    false otherwise. *)

val fullP : bitmap -> bool
(** [Bitmap.fullP x] returns true if no bit is clear in bitmap [x],
    false otherwise. *)

val equalP : bitmap -> bitmap -> bool
(** [Bitmap.equalP x y] returns true, if the bitmaps [x] and [y]
    have the same length and are element-wise equal, false otherwise. *)

val bitsetP : bitmap -> int -> bool
(** [Bitmap.bitsetP x i] returns true if the i-th bit in bitmap [x] is set,
    false otherwise.  Raises [Bad_Index] if [i] is out of range. *)

val setbit : bitmap -> int -> unit
(** [Bitmap.setbit x i] destructively sets the i-th bit in bitmap [x].
    Raises [Bad_Index] if [i] is out of range. *)

val clearbit : bitmap -> int -> unit
(** [Bitmap.clearbit x i] destructively clears the i-th bit in bitmap [x].
    Raises [Bad_Index] if [i] is out of range. *)

val weight : bitmap -> int
(** [Bitmap.weight x] returns the number of set bits in [x]. *)

val to_string : bitmap -> bytes
(** [Bitmap.to_string x] returns the textual representation of bitmap [x]. *)

val from_string : bytes -> bitmap
(** [Bitmap.from_string str] returns the bitmap corresponding to the
    string [str]. 
    Raises [Bad_String] if the string is empty
    or if the string contains characters different from '0' and '1'. *)


(* UNSAFE FUNCTIONS (function names end "'") *)
val bitsetP' : bitmap -> int -> bool
(** [Bitmap.bitsetP' x i] returns true if the i-th bit in bitmap [x] 
    is set, false otherwise. *)

val blit' : bitmap -> bitmap -> bitmap
(** [Bitmap.blit' src dst] blits the contents of bitmap [src] 
    into bitmap [dst] and returns [dst]. *)

val setbit' : bitmap -> int -> unit
(** [Bitmap.setbit' x i] destructively sets the i-th bit in [x]. *)

val clearbit' : bitmap -> int -> unit
(** [Bitmap.clearbit' x i] destructively clears the i-th bit in [x]. *)
