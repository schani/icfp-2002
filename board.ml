(** board.mli
    interface-file for board.ml

    All usable indices of boards are in the range 1..N.

    Status:
	[1] Sat Aug 31 10:58:14 CEST 2002:
		- created 
 **)

open Bitmap
open Bitmap2d

(** A board with x columns and y rows is a 2d-bitmap of size x*y. 

    Every cell of a board is represented as two y-adjacent bits.
	e.g., 	somecell = firstbit secondbit

		C_Plain    = 0 0
		C_Homebase = 0 1
		C_Water    = 1 0
		C_Wall     = 1 1
*)

type board = bitmap2d

type cell =
  | C_Plain
  | C_Water
  | C_Wall
  | C_Homebase


exception Bad_String

let emit s = print_string s; flush Pervasives.stdout

(**********************************************************************)

let cell_to_char = function
  | C_Homebase -> '@'
  | C_Plain -> '.'
  | C_Water -> '~'
  | C_Wall -> '#'

let char_to_cell = function
  | '.' -> C_Plain
  | '~' -> C_Water
  | '#' -> C_Wall
  | '@' -> C_Homebase
  | _   -> raise Bad_String

let copy b = Bitmap2d.copy b

let equalP bm1 bm2 = Bitmap2d.equalP bm1 bm2

let getcell' b x y =
  let x2 = x*2 in
  if bitsetP' b x2 y then if bitsetP' b (x2+1) y then C_Wall else C_Water
  else if bitsetP' b (x2+1) y then C_Homebase else C_Plain

let getcell_in_row' row x =
  let x2 = x*2 in
  if Bitmap.bitsetP' row x2 then 
    if Bitmap.bitsetP' row (x2+1) then C_Wall else C_Water
  else 
    if Bitmap.bitsetP' row (x2+1) then C_Homebase else C_Plain

let setcell' b x y = function
  | C_Plain    -> clearbit' b (x*2) y; clearbit' b (x*2+1) y
  | C_Homebase -> clearbit' b (x*2) y; setbit' b (x*2+1) y
  | C_Water    -> setbit' b (x*2) y; clearbit' b (x*2+1) y
  | C_Wall     -> setbit' b (x*2) y; setbit' b (x*2+1) y

let setcell_in_row' b x = function
  | C_Plain    -> Bitmap.clearbit' b (x*2); Bitmap.clearbit' b (x*2+1)
  | C_Homebase -> Bitmap.clearbit' b (x*2); Bitmap.setbit' b (x*2+1)
  | C_Water    -> Bitmap.setbit' b (x*2); Bitmap.clearbit' b (x*2+1)
  | C_Wall     -> Bitmap.setbit' b (x*2); Bitmap.setbit' b (x*2+1)

let cannot_visit_cell' b x y = bitsetP' b (x*2) y

let make_empty' x y =
  let b = Bitmap2d.make_empty ((x+2)*2) (y+2) in
  for i = 0 to x + 1 do setcell' b i 0 C_Wall; setcell' b i (y+1) C_Wall done;
  for i = 0 to y + 1 do setcell' b 0 i C_Wall; setcell' b (x+1) i C_Wall done;
  b

let make_empty_row i = Bitmap.make_empty (i*2)

let row_from_string' l s = 
  if l = 0 then raise Bad_String
  else 
    let dst = make_empty_row (l+2) in
      setcell_in_row' dst 0     C_Wall;
      setcell_in_row' dst (l+1) C_Wall;
      for i = l downto 1 do
	setcell_in_row' dst i (char_to_cell s.[i-1])
      done;
      dst

(* The row returned is delimited by walls on the west and on 
   the east side. *)
let row_from_string s = row_from_string' (String.length s) s

let blit_row' bm y bm2d = Bitmap2d.blit_row' bm y bm2d

let fetch_row' bm2d y bm = Bitmap2d.fetch_row' bm2d y bm

let row_to_string bm = 
  let n = ((Bitmap.size bm) / 2) - 2 in
  let s = String.make n '.' in
    for i = 1 to n do
      s.[i-1] <- cell_to_char (getcell_in_row' bm i)
    done;
    s

let row_to_string' bm = 
  let n = (Bitmap.size bm) / 2 in
  let s = String.make n '.' in
    for i = 0 to (n-1) do
      s.[i] <- cell_to_char (getcell_in_row' bm i)
    done;
    s
	
let rawsizeX bm = Bitmap2d.sizeX bm / 2
let rawsizeY bm = Bitmap2d.sizeY bm

let sizeX bm = rawsizeX bm - 2
let sizeY bm = rawsizeY bm - 2

