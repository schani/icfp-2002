open Bitmap
open Array

type bitmap2d = 
  | BM2d of 
	int *				(* # of columns (X-dimension) 	*) 
	int *				(* # of rows (Y-dimension) 	*)
	bitmap array			(* data store			*)

exception Bad_BitmapSize
exception Bad_Index


let max_bitmap_size = 32768

let valid_sizeP x y = Bitmap.valid_sizeP x && Bitmap.valid_sizeP y

let make_empty x y = 
  if not (valid_sizeP x y) then raise Bad_BitmapSize
  else BM2d(x, y, Array.init y (fun _ -> Bitmap.make_empty x))

let make_full x y = 
  if not (valid_sizeP x y) then raise Bad_BitmapSize
  else BM2d(x, y, Array.init y (fun _ -> Bitmap.make_full x))

let inrangeP (BM2d(c,r,_)) x y = x >= 0 && x < c && y >= 0 && y < r

let sizeX (BM2d(x,_,_)) = x
let sizeY (BM2d(_,y,_)) = y

let copy (BM2d(x,y,bm)) = BM2d(x,y,Array.init y (fun i -> Bitmap.copy bm.(i)))

let clear (BM2d(w,h,bm)) =
  for y = h-1 downto 1 do
    Bitmap.clear bm.(y)
  done

let equalP (BM2d(x1,y1,s1)) (BM2d(x2,y2,s2)) =
  x1 = x2 && y1 = y2 &&
    let retval = ref true
    and i = ref (y1-1) in
      while !i >= 0 && !retval do
	retval := Bitmap.equalP s1.(!i) s2.(!i);
	decr i
      done;
      !retval

let emptyP (BM2d(x,y,s)) =
  let retval = ref true
  and i = ref (y-1) in
    while !i >= 0 && !retval do
      retval := Bitmap.emptyP s.(!i);
      decr i
    done;
    !retval

let fullP (BM2d(x,y,s)) =
  let retval = ref true
  and i = ref (y-1) in
    while !i >= 0 && !retval do
      retval := Bitmap.fullP s.(!i);
      decr i
    done;
    !retval

let weight (BM2d(_,y,s)) =
  let w = ref (Bitmap.weight s.(0)) in
  for i = y-1 downto 1 do
    w := !w + Bitmap.weight s.(i)
  done;
  !w

let row_to_string' (BM2d(c,r,b)) y = Bitmap.to_string b.(y)

let to_string (BM2d(x,y,s)) = 		(* not efficient. use string-buffer? *)
  let rec loop i = 
    if i = 0 then Bitmap.to_string s.(i)
    else Bytes.cat (loop (i-1)) (Bitmap.to_string s.(i))
  in loop (y-1)				(* y-1 >= 0 *)

let bitsetP' (BM2d(_,_,s)) x y = Bitmap.bitsetP' s.(y) x 

let setbit' (BM2d(_,_,sd)) x y = Bitmap.setbit' sd.(y) x

let clearbit' (BM2d(_,_,sd)) x y = Bitmap.clearbit' sd.(y) x

let bitsetP bm x y = 
  if inrangeP bm x y then bitsetP' bm x y
  else raise Bad_Index

let setbit bm x y = 
  if inrangeP bm x y then setbit' bm x y 
  else raise Bad_Index

let clearbit bm x y = 
  if inrangeP bm x y then clearbit' bm x y
  else raise Bad_Index

let blit_row' row y (BM2d(c,r,dst) as d) = 
  ignore (Bitmap.blit' row dst.(y)); d

let fetch_row' (BM2d(c,r,src) as s) y row = 
  ignore (Bitmap.blit' src.(y) row); row


let blit_row row y (BM2d(c,r,dst) as d) =
  if not (y >= 0 && y < r) then raise Bad_BitmapSize
  else ignore (Bitmap.blit row dst.(y)); d


