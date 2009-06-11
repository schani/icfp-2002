open Array 
open Pos
open Board
open Client
open Bitmap2d


type sssps =
  SssPs of 
	board *
	(int array) array *
	(int array) array *
	(int array) array *
	(pos array) array *
	bitmap2d


(* INITIALIZATION ***********************************************************)

let dijkstra_init board outside_cost eval_static =
  let w = Board.rawsizeX board
  and h = Board.rawsizeY board in

  let eval_values = init h (fun _ -> make w 0) in

  let eval_static_maybe_outside x' y' d = 
    if x' >= 0 && x' < w && y' >= 0 && y' < h then eval_static x' y' d
    else outside_cost d in

  (* static evaluation of fields; skip walls surrounding the board *)
  for y = 1 to h - 2 do
    let row = eval_values.(y) in
    for x = 1 to w - 2 do
      if cannot_visit_cell' board x y then ()
      else
        let v = 
          if (x-2) >= 0 && (y-2) >= 0 && 
	     (x+2)  < w && (y+2)  < h 
	  then 						(* all inside *)
	    (eval_static   x   (y+2) 2) +
	    (eval_static (x-1) (y+1) 2) + 
	    (eval_static   x   (y+1) 1) + 
	    (eval_static (x+1) (y+1) 2) +
	    (eval_static (x-2)   y   2) +
	    (eval_static (x-1)   y   1) + 
	    (eval_static   x     y   0) +
	    (eval_static (x+1)   y   1) +
	    (eval_static (x+2)   y   2) +
	    (eval_static (x-1) (y-1) 2) + 
	    (eval_static   x   (y-1) 1) +
	    (eval_static (x+1) (y-1) 2) +
	    (eval_static   x   (y-2) 2)
          else
	    (eval_static_maybe_outside   x   (y+2) 2) +
	    (eval_static_maybe_outside (x-1) (y+1) 2) + 
	    (eval_static_maybe_outside   x   (y+1) 1) + 
	    (eval_static_maybe_outside (x+1) (y+1) 2) +
	    (eval_static_maybe_outside (x-2)   y   2) +
	    (eval_static_maybe_outside (x-1)   y   1) + 
	    (eval_static_maybe_outside   x     y   0) +
	    (eval_static_maybe_outside (x+1)   y   1) +
	    (eval_static_maybe_outside (x+2)   y   2) +
	    (eval_static_maybe_outside (x-1) (y-1) 2) + 
	    (eval_static_maybe_outside   x   (y-1) 1) +
	    (eval_static_maybe_outside (x+1) (y-1) 2) +
	    (eval_static_maybe_outside   x   (y-2) 2)
        in row.(x) <- v
    done
  done;

  let d = init h (fun _ -> make w max_int) in		 (* path-lens *)
  let pi = init h (fun _ -> make w (make_unknown ())) in (* preds *)
  let final = Bitmap2d.make_empty w h in		 (* visited *)
  let eval_values_tmp = init h (fun _ -> make w max_int) in
    SssPs (board, eval_values, eval_values_tmp, d, pi, final)


(* DIJKSTRA'S SINGLE SOURCE SHORTEST PATH ALGORITHM *************************)
let dijkstra 
	(SssPs(board,eval_tbl,eval_tbl',d,pi,final)) 
	start_x start_y					(* starting point *)
	bonus_malus =					(* reward/threatening info *)
  let w = Board.rawsizeX board
  and h = Board.rawsizeY board in

  for y = h - 1 downto 1 do				(* initialize d *)
    let row_d  = d.(y) 
    and row_pi = pi.(y) in
      for x = w - 1 downto 1 do
        row_d.(x)  <- max_int;
        row_pi.(x) <- make_unknown ()
      done
  done;

  for y = h - 1 downto 1 do				(* make copy of tbl *)
    let row_eval = eval_tbl.(y)
    and row_evaltmp = eval_tbl'.(y) in
      for x = w - 1 downto 1 do row_evaltmp.(x) <- row_eval.(x) done
  done;

  let add_dynamic_costs' eval x y d =
    if x <= 0 || x >= (w-1) || 
       y <= 0 || y >= (h-1) || 
       cannot_visit_cell' board x y
    then ()
    else (eval_tbl'.(y)).(x) <- (eval_tbl'.(y)).(x) + eval d in

  let rec add_bonus_malus = function  			(* add info to tbl *)
    | [] -> ()
    | ((x,y),eval)::bms ->
	add_dynamic_costs' eval   x   (y+2) 2;
	add_dynamic_costs' eval (x-1) (y+1) 2; 
	add_dynamic_costs' eval   x   (y+1) 1; 
	add_dynamic_costs' eval (x+1) (y+1) 2;
	add_dynamic_costs' eval (x-2)   y   2;
	add_dynamic_costs' eval (x-1)   y   1; 
	add_dynamic_costs' eval   x     y   0;
	add_dynamic_costs' eval (x+1)   y   1;
	add_dynamic_costs' eval (x+2)   y   2;
	add_dynamic_costs' eval (x-1) (y-1) 2; 
	add_dynamic_costs' eval   x   (y-1) 1;
	add_dynamic_costs' eval (x+1) (y-1) 2;
	add_dynamic_costs' eval   x   (y-2) 2;
          add_bonus_malus bms in

  add_bonus_malus bonus_malus;				(* update eval_tbl' *)

  (d.(start_y)).(start_x) <- 0;				(* set initial dist *)
  Bitmap2d.clear final;					(* initialize final *)
  Bitmap2d.setbit' final start_x start_y;		(* mark visited *)

  let add_to_pq q v predecessor x y =
    if Bitmap2d.bitsetP' final x y || cannot_visit_cell' board x y then q
    else 
       let v' = v + (eval_tbl'.(y)).(x)
       and row = d.(y) in
       if v' >= row.(x) then q 
       else
       begin
         row.(x) <- v';
	 (pi.(y)).(x) <- predecessor;
         Pqueue.insert q v' (make_pos x y) 
       end in

  let add_neighbours q v xy_pos x y =
    let q = add_to_pq q v xy_pos (x-1) y in 
    let q = add_to_pq q v xy_pos (x+1) y in
    let q = add_to_pq q v xy_pos x (y-1) in
            add_to_pq q v xy_pos x (y+1) in

  let rec loop q =
    if Pqueue.emptyP q then () (* SssPs(d,pi) *)
    else 
      let (v,p,q') = Pqueue.extract q in
        let x = get_x p
        and y = get_y p in
          Bitmap2d.setbit' final x y;
	  (d.(y)).(x) <- v;
            loop (add_neighbours q' v p x y) in
  loop (Pqueue.insert Pqueue.empty 0 (make_pos start_x start_y))



let dump_mindists (SssPs(_,_,_,d,_,_)) =
  Array.iter 
	(fun row -> 
	   Printf.printf "\n   ";
	   Array.iter 
		(fun i -> if  i = max_int then print_string "www " else Printf.printf "%03d " i)
		row) d;
  flush stdout

let mindist_to (SssPs(_,_,_,d,_,_)) x y = (d.(y)).(x)

let shortestpath_to (SssPs(_,_,_,_,pi,_)) = 
  let rec pi_to_path' x y acc = 
    let pred_pos = (pi.(y)).(x) in
      if not (knownP pred_pos) then acc
      else 
        let p_x = get_x pred_pos  
        and p_y = get_y pred_pos in
          let dir =
            if p_x = x then
              if p_y = (y-1) then North
	      else if p_y = (y+1) then South
	      else failwith "pi_to_path' (1)"
	    else
	      if p_y <> y then failwith "pi_to_path' (2)"
	      else
	        if p_x = (x-1) then East
	        else if p_x = (x+1) then West
	        else failwith "pi_to_path' (3)" in
          pi_to_path' p_x p_y (dir::acc) in
  fun x y ->
    pi_to_path' x y []
