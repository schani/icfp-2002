(* client.ml
   Manage communication with server. *)

open List
open Unix
open Board

type direction = 
  | North 
  | South 
  | West
  | East

type robot_id = int

type position = int * int

type package_id = int
type package = 
    Pkg of 
	package_id * 				(* identifier *)
	position option *			(* current position *)
	position option * 			(* destination *)
	int option				(* weight *)

type initstatus = 
    IStatus of 
	robot_id * 
	int * 
	int * 
	position * 
	(robot_id * position) list * 
	board * 
	package list * 
	(in_channel * out_channel)

type command = 
  | Move of direction
  | Pick of package_id list
  | Drop of package_id list

type reaction = 
  | MovedRel of robot_id * direction
  | MovedAbs of robot_id * position
  | Picked of robot_id * package_id
  | Dropped of robot_id * package_id


let packageid_to_string id = string_of_int id
let robotid_to_string id = string_of_int id

let packageid_of_string str = int_of_string str
let robotid_of_string str = int_of_string str

let direction_to_char = function
  | North -> 'N'
  | South -> 'S'
  | West  -> 'W'
  | East  -> 'E'

let char_to_direction = function
  | 'N' -> North
  | 'S' -> South
  | 'W' -> West
  | 'E' -> East
  | _ -> failwith "char_to_direction"


let input_line' description in' = 
  let str = input_line in' in
    Printf.printf "\n[%s] > %s\n" description str;
  str


(* LEXING TABLES *************************************************************)

let is_digit_tbl =
  let tbl = Array.make 256 false in
  tbl.(Char.code '0') <- true;
  tbl.(Char.code '1') <- true;
  tbl.(Char.code '2') <- true;
  tbl.(Char.code '3') <- true;
  tbl.(Char.code '4') <- true;
  tbl.(Char.code '5') <- true;
  tbl.(Char.code '6') <- true;
  tbl.(Char.code '7') <- true;
  tbl.(Char.code '8') <- true;
  tbl.(Char.code '9') <- true;
  tbl

let is_whitespace_tbl =
  let tbl = Array.make 256 false in
  tbl.(Char.code ' ') <- true;
  tbl.(Char.code '\t') <- true;
  tbl.(Char.code '\n') <- true;
  tbl

let digit_to_int_tbl = 
  let tbl = Array.make 256 0 in
  tbl.(Char.code '0') <- 0;
  tbl.(Char.code '1') <- 1;
  tbl.(Char.code '2') <- 2;
  tbl.(Char.code '3') <- 3;
  tbl.(Char.code '4') <- 4;
  tbl.(Char.code '5') <- 5;
  tbl.(Char.code '6') <- 6;
  tbl.(Char.code '7') <- 7;
  tbl.(Char.code '8') <- 8;
  tbl.(Char.code '9') <- 9;
  tbl


(* CHAR CLASSIFICATION ******************************************************)

let is_whitespace c = is_whitespace_tbl.(Char.code c)

let is_digit c = is_digit_tbl.(Char.code c)

(* precondition: c is a character representing a decimal digit *)
let digit_to_int' c = digit_to_int_tbl.(Char.code c) 

let digit_to_int = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | _ -> failwith "digit_to_int"


let times_10 i = i lsl 3 + i lsl 1
(*
let times_10 i = i * 10

-- The native code compiler for x86 (ocaml-3.06) produces a 
   'imull'-instruction for this, which is _very_ bad on the Pentium 4.
*)


(* LEXING BASICS *************************************************************)

let rec decnum_0p str i p0 p =
  if p0 > p then (p0,i)
  else 
    let c = str.[p0] in
    if not (is_digit c) then (p0,i)
    else decnum_0p str (times_10 i + (digit_to_int' c)) (p0+1) p

let decnum_1p str p0 p =
  if p0 > p then failwith "decnum_1p (1)"
  else 
    let c = str.[p0] in
    if not (is_digit c) then failwith "decnum_1p (2)"
    else decnum_0p str (digit_to_int' c) (p0+1) p

let rec ws_0p str p0 p =
  if p0 > p || not (is_whitespace str.[p0]) then p0
  else ws_0p str (p0+1) p

let ws_1p str p0 p =
  if p0 > p then failwith "ws_1p (1)"
  else if not (is_whitespace str.[p0]) then failwith "ws_1p (2)"
  else ws_0p str (p0+1) p

let hash_1 str p0 p =
  if p0 > p then failwith "hash_1 (1)"
  else if str.[p0] <> '#' then failwith "hash_1 (2)" 
  else p0+1

let bigx_1 str p0 p =
  if p0 > p then failwith "bigx_1 (1)"
  else if str.[p0] <> 'X' then failwith "bigx_1 (2)" 
  else p0+1

let bigy_1 str p0 p =
  if p0 > p then failwith "bigy_1 (1)"
  else if str.[p0] <> 'Y' then failwith "bigy_1 (2)" 
  else p0+1

(* PARSING *******************************************************************)

let string_to_pkgs str =
  let rec string_to_pkgs' acc p0 p =
    if p0 > p then acc
    else 
      let (p1,pkg_id) = decnum_1p str p0 p in		(* [0-9]+ *)
      let p2 = ws_1p str p1 p in			(* [ ]+   *)
      let (p3,x) = decnum_1p str p2 p in		(* [0-9]+ *)
      let p4 = ws_1p str p3 p in			(* [ ]+   *)
      let (p5,y) = decnum_1p str p4 p in		(* [0-9]+ *)
      let p6 = ws_1p str p5 p in			(* [ ]+   *)
      let (p7,w) = decnum_1p str p6 p in		(* [0-9]+ *)
      let p8 = ws_0p str p7 p in			(* [ ]*   *)
	string_to_pkgs' (Pkg(pkg_id, None, Some(x,y), Some w)::acc) p8 p in
  let p = String.length str - 1 in
    string_to_pkgs' [] (ws_0p str 0 p) p		(* [ ]*   *)

let string_to_playerlocs str =
  let rec string_to_playerlocs' acc p0 p =
    if p0 > p then acc
    else
      let p1 = hash_1 str p0 p in			(* # 	  *)
      let (p2,id) = decnum_1p str p1 p in		(* [0-9]+ *)
      let p3 = ws_1p str p2 p in			(* [ ]+   *)
      let p4 = bigx_1 str p3 p in			(* X 	  *)
      let p5 = ws_1p str p4 p in			(* [ ]+   *)
      let (p6,x) = decnum_1p str p5 p in		(* [0-9]+ *)
      let p7 = ws_1p str p6 p in			(* [ ]+   *)
      let p8 = bigy_1 str p7 p in			(* Y      *)
      let p9 = ws_1p str p8 p in			(* [ ]+   *)
      let (p10,y) = decnum_1p str p9 p in		(* [0-9]+ *)
      let p11 = ws_0p str p10 p in			(* [ ]*   *)
       string_to_playerlocs' ((id,(x,y))::acc) p11 p in
  let p = String.length str - 1 in
    string_to_playerlocs' [] (ws_0p str 0 p) p	(* [ ]*   *)

let string_to_boardsize str =
  let p = String.length str - 1 in
  let p0 = 0 in
  let p1 = ws_0p str p0 p in				(* [ ]*   *)
  let (p2,w) = decnum_1p str p1 p in			(* [0-9]+ *)
  let p3 = ws_1p str p2 p in				(* [ ]+   *)
  let (p4,h) = decnum_1p str p3 p in			(* [0-9]+ *)
  let p5 = ws_0p str p4 p in				(* [ ]*   *)
    if p5 > p then (w,h)				(* done   *)
    else failwith "string_to_boardsize"

let string_to_playercfg str =
  let p = String.length str - 1 in
  let p0 = 0 in
  let p1 = ws_0p str p0 p in				(* [ ]*   *)
  let (p2,id) = decnum_1p str p1 p in			(* [0-9]+ *)
  let p3 = ws_1p str p2 p in				(* [ ]+   *)
  let (p4, max_carry) = decnum_1p str p3 p in		(* [0-9]+ *)
  let p5 = ws_1p str p4 p in				(* [ ]+   *)
  let (p6, money) = decnum_1p str p5 p in		(* [0-9]+ *)
  let p7 = ws_0p str p6 p in				(* [ ]*   *)
    if p7 > p then (id, max_carry, money)		(* done   *)
    else failwith "string_to_playercfg"

let string_to_answers str =
  let rec string_to_answers' acc p0 p =
    if p0 > p then acc
    else
      let p1 = hash_1 str p0 p in			(* # 	  *)
      let (p2,id) = decnum_1p str p1 p in		(* [0-9]+ *)
      let p3 = ws_0p str p2 p in			(* [ ]*   *)
	if p3 > p then acc				(* done   *)
        else string_to_answers2' id acc p3 p
  and string_to_answers2' id acc p0 p =
    if p0 > p then acc
    else
      let p1 = p0 + 1 in
      let c = str.[p0] in
        match c with
          | 'N' | 'S' | 'W' | 'E' -> 
	      let p2 = ws_0p str p1 p in
	      let answer = MovedRel(id,char_to_direction c) in
	        if p2 > p then answer::acc
		(* else if p2=p1 then failwith "string_to_answers2' (1)" *)
		else string_to_answers2' id (answer::acc) p2 p
	  | 'X' ->
	      let p2 = ws_1p str p1 p in		(* [ ]+   *)
	      let (p3,x) = decnum_1p str p2 p in	(* [0-9]+ *)
	      let p4 = ws_1p str p3 p in		(* [ ]+   *)
	      let p5 = bigy_1 str p4 p in		(* Y	  *)
	      let p6 = ws_1p str p5 p in		(* [ ]+   *)
	      let (p7,y) = decnum_1p str p6 p in	(* [0-9]+ *)
	      let p8 = ws_0p str p7 p in		(* [ ]*   *)
		let answer = MovedAbs(id,(x,y)) in
	        if p8 > p then answer::acc
		(* else  if p8=p7 then failwith "string_to_answers2' (2)" *)
		else string_to_answers2' id (answer::acc) p8 p
	  | '#' ->
	      string_to_answers' acc p0 p
	  | 'P' | 'D' ->
	      let p2 = ws_1p str p1 p in		(* [ ]+   *)
	      let (p3,pkg) = decnum_1p str p2 p in	(* [0-9]+ *)
	      let p4 = ws_0p str p3 p in		(* [ ]*   *)
		let answer = 
		  if c = 'P' then Picked(id,pkg) else Dropped(id,pkg) in
                if p4 > p then answer::acc
		(* else if p4=p then failwith (Printf.sprintf "string_to_answers2' (3) %d" (Char.code str.[p4]))  *)
		else string_to_answers2' id (answer::acc) p4 p
	  | _ ->
	      failwith "string_to_answers' (4)" in
  let p = String.length str - 1 in
    rev (string_to_answers' [] (ws_0p str 0 p) p) 	(* [ ]*   *)


(* UNPARSING ****************************************************************)

let unparse_buffer = Buffer.create 100

let rec add_packagelist_to_unparse_buffer = function
  | [] -> ()
  | p::ps ->
      Buffer.add_string unparse_buffer (packageid_to_string p);
      Buffer.add_char unparse_buffer ' ';
      add_packagelist_to_unparse_buffer ps 

let request_to_string bid cmd =
  Buffer.reset unparse_buffer;
  Buffer.add_string unparse_buffer (string_of_int bid);
  Buffer.add_char unparse_buffer ' ';
    (match cmd with
      | Move dir -> 
	  Buffer.add_string unparse_buffer "Move ";
	  Buffer.add_char unparse_buffer (direction_to_char dir)
      | Pick ps -> 
	  Printf.printf "pick. len=%d.\n" (List.length ps);
	  Buffer.add_string unparse_buffer "Pick ";
	  add_packagelist_to_unparse_buffer ps
      | Drop ps -> 
	  Printf.printf "drop. len=%d.\n" (List.length ps);
	  Buffer.add_string unparse_buffer "Drop ";
	  add_packagelist_to_unparse_buffer ps);
     Buffer.add_char unparse_buffer '\n';
     Buffer.contents unparse_buffer

  
(* INTERACTION CYCLE *********************************************************)

let read_pkgs_at_curloc in' =
  string_to_pkgs (input_line' "read_pkgs" in')

let read_playerlocs in' =
  string_to_playerlocs (input_line' "read_playerlocs" in')

let read_playercfg in' = 
  string_to_playercfg (input_line' "read_playercfg" in')

let read_board in' =
  let (w,h) = string_to_boardsize (input_line' "read_board" in') in
  let board = Board.make_empty' w h in
    for y = 1 to h do
      let input_str = input_line in' in
      let row = Board.row_from_string' w input_str in
        ignore (Board.blit_row' row y board)
    done;
    board

let read_answer in' =
  string_to_answers (input_line' "rcv_answer" in')

let initialize server port =
  let (in',out') as s = 
    let host = gethostbyname server in
    open_connection (ADDR_INET(host.h_addr_list.(0), port)) in
  output_string out' 
	(if Array.length Sys.argv > 3 then 
	 	(Printf.sprintf "Login %s %s\n" Sys.argv.(3) Sys.argv.(4))
         else 
		"Player\n");
  flush out';
  let board = read_board in' in
  let (id,max_carry,money) = read_playercfg in' in
  let (own_loc, other_locs) =
    match partition (fun (x,_) -> x=id) (read_playerlocs in') with
      | ([(_,own_loc)], other_locs) -> (own_loc, other_locs)
      | _ -> failwith "initialize: illegal position list!"
  in
  let pkgs_at_curloc = read_pkgs_at_curloc in' in
    IStatus(id,max_carry,money,own_loc,other_locs,board,pkgs_at_curloc,s)

let send_request out' bid cmd =
  let str = request_to_string bid cmd in
    Printf.printf "< %s" str;
    flush Pervasives.stdout;
    output_string out' str;
    flush out'

let do_turn (in',out') (bid,cmd) = 
  send_request out' bid cmd;
  let response = read_answer in' in
  let pkgs_at_curloc = read_pkgs_at_curloc in' in
    (pkgs_at_curloc, response)
    


