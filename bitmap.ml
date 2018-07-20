(** bitmap.ml
    generic support for bitmaps (vectors of booleans).

    This module implements bitmaps based on the basic data-type String.

    Status:
	[1] Sat Aug 31 08:39:50 CEST 2002
		- created
 **)

open Char
open Bytes

type bitmap = 
  | BM of 
	int * 				(* size of the bitmap (# of bits)   *)
	int *				(* index of last word used in array *)
	bytes				(* chars for storing data 	    *)
					(*	. easy EA calculation	    *)
					(*      . last char is zero-padded  *)

exception Bad_BitmapSize		(* blit/make_full/make_empty 	    *)
exception Bad_Index			(* bitsetP, setbit, clearbit 	    *)
exception Bad_String			(* from_string			    *)

(** INTERNAL FUNCTIONS ******************************************************)

let shift = 3 				(* use 8 bits per char *)

let max_bitmapbits_per_int = 1 lsl shift

let wordidx_of_bitidx i = i lsr shift

let bitmask_of_bitidx i = 1 lsl (i land ((1 lsl shift) - 1))

let fullmask_int = (1 lsl max_bitmapbits_per_int - 1)
let fullmask = unsafe_chr (1 lsl max_bitmapbits_per_int - 1)

let lastfullmask_int n = (bitmask_of_bitidx (n-1) lsl 1) - 1
let lastfullmask n = unsafe_chr ((bitmask_of_bitidx (n-1) lsl 1) - 1)

let words_for_bits n = (n + max_bitmapbits_per_int - 1) lsr shift


(** EXPORTED FUNCTIONS ******************************************************)

let max_bitmap_size = 32768

let valid_sizeP n = n > 0 && n <= max_bitmap_size

let make_empty n = 
  if not (valid_sizeP n) then raise Bad_BitmapSize
  else
    let s = words_for_bits n in 
      BM(n, s - 1, make s '\000')

let make_full n = 
  if not (valid_sizeP n) then raise Bad_BitmapSize
  else
    let s = words_for_bits n in
      let arr = make s fullmask in
        Bytes.set arr (s - 1) (lastfullmask n);
        BM(n,s - 1,arr)

let size (BM(s,_,_)) = s

let inrangeP bm i = i >= 0 && i < (size bm)

let eqsize2P x y = size x = size y

let copy (BM(s,w,arr)) = BM(s,w,Bytes.copy arr)

let clear (BM(_,j,sd)) =
  Bytes.set sd 0 '\000';					(* ref: s, d *)
  for i = j downto 1 do					(* ref: j *)
    Bytes.set sd i '\000'
  done

let equalP (BM(n1,j,s1)) (BM(n2,_,s2)) =
  let i = ref j
  and retval = ref (n1 = n2 && (Bytes.get s1 0) = (Bytes.get s2 0)) in	(* ref: s1, s2 *)
    while !retval && !i > 0 do
      retval := (Bytes.get s1 !i) = (Bytes.get s2 !i);
      decr i
    done;
    !retval

let emptyP (BM(_,j,s)) =
  let i = ref j						(* ref: j *)
  and retval = ref ((Bytes.get s 0) = '\000') in			(* ref: s *)
    while !retval && !i > 0 do				(* skip first word *)
      retval := (Bytes.get s !i) = '\000';
      decr i
    done;
    !retval

let fullP (BM(n,j,s)) =
  let i = ref (j-1)					(* ref: j *)
  and retval = ref (lastfullmask n = (Bytes.get s j)) in		(* ref: s, n *)
    while !retval && !i >= 0 do	    			(* skip last word *)
      retval := (Bytes.get s !i) = fullmask;
      decr i
    done;
    !retval

let bitsetP' (BM(_,_,s)) i =
  ignore s;						(* ref: s *)
  (code (Bytes.get s (wordidx_of_bitidx i)) land (bitmask_of_bitidx i)) <> 0

let setbit' (BM(_,_,sd)) i =
  let wordidx = wordidx_of_bitidx i in
  let old_contents = code (Bytes.get sd wordidx) in
  let bitmask = bitmask_of_bitidx i in
    Bytes.set sd wordidx (unsafe_chr (old_contents lor bitmask))

let clearbit' (BM(_,_,sd)) i =
  let wordidx = wordidx_of_bitidx i in
  let old_contents = code (Bytes.get sd wordidx) in
  let bitmask = bitmask_of_bitidx i in
    Bytes.set sd wordidx (unsafe_chr (old_contents land (lnot bitmask)))

let blit' (BM(_,j,s)) (BM(_,_,d) as dst) =
  Bytes.set d 0 (Bytes.get s 0);					(* ref: s, d *)
  for i = j downto 1 do					(* ref: j *)
    Bytes.set d i (Bytes.get s i)
  done;
  dst

let bitsetP bm i = if inrangeP bm i then bitsetP' bm i else raise Bad_Index

let setbit bm i = if inrangeP bm i then setbit' bm i else raise Bad_Index

let clearbit bm i = if inrangeP bm i then clearbit' bm i else raise Bad_Index

let blit s d = if eqsize2P s d then blit' s d else raise Bad_BitmapSize

(* assumes that x in 0..255 *)
let weight8_of_int x =
  let x = (x land 0x55) + ((x lsr 1) land 0x55) in
  let x = (x land 0x33) + ((x lsr 2) land 0x33) in
    (x land 0x0F) + ((x lsr 4) land 0x0F)

let weight (BM(_,j,s)) =
  let w = ref (weight8_of_int (code (Bytes.get s 0))) in		(* ref: s *)
  for i = j downto 1 do					(* ref: j *)
    w := !w + weight8_of_int (code (Bytes.get s i))
  done;
  !w

let to_string (BM(n,j,s) as bm) = 
  let str = Bytes.make n '0' in			(* ref: n, str *)
  ignore s;						(* ref: s *)
  ignore j;						(* ref: j *)
    for i = n - 1 downto 0 do
      Bytes.set str i (if bitsetP' bm i then '1' else '0')
    done;
    str

let from_string s = 
  let l = Bytes.length s in				(* ref: s *)
    if l = 0 then raise Bad_String
    else 
      let (BM(_,_,d) as dst) = make_empty l in		(* ref: dst *)
	ignore s;					(* ref: s *)
	ignore d;					(* ref: d *)
        for i = l - 1 downto 0 do
	  match Bytes.get s i with
	    | '0' -> ()
	    | '1' -> setbit' dst i
	    | _   -> raise Bad_String
	done;
	dst

