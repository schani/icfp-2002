open Random
open Printf
open Client
open Dijkstra
open Bitmap2d
open Board
open List
open Unix

exception Hell

type search_type =
    ForeignVisitedHomebase
  | UnvisitedHomebase
  | SafePacket
  | UnsafePacket

type robot =
    { robot_id : robot_id;
      robot_position : position option;
      robot_packages : package_id list }

type local_strategy =
    Dodge of direction
  | LocalSearch of position
  | ExchangePackages of package_id list * package_id list (* drops, picks *)

type global_strategy =
    PackageSearch of position
  | PackageDelivery of package_id

type game =
    { self_id : robot_id;
      self_position : position;
      self_packages : package_id list;
      self_money : int;
      self_capacity : int;
      dodge_timeout : int;
      present_packages : package_id list;
      game_board : board;
      path_map : sssps;
      self_visited : bitmap2d;
      foreign_visited : bitmap2d;
      game_packages : package list;
      average_weight : int;
      game_server : (in_channel * out_channel);
      game_robots : robot list;
      global_strategy : global_strategy option }

(* UNPARSER start *)

let unparse_buffer = Buffer.create 100

let rec add_packageidlist_to_unparse_buffer = function
  | x1::x2::x3::x4::x5::xs ->
	Buffer.add_string 
		unparse_buffer 
		(Printf.sprintf "		%d %d %d %d %d\n" x1 x2 x3 x4 x5);
	add_packageidlist_to_unparse_buffer xs
  | xs ->
	Buffer.add_string unparse_buffer "		";
        List.iter (fun i -> Buffer.add_string unparse_buffer (Printf.sprintf "%d " i)) xs;
	Buffer.add_char unparse_buffer '\n'

let pos_option_to_string = function
  | Some(x,y) -> Printf.sprintf "(x=%d,y=%d)" x y
  | None -> "None"

let int_option_to_string = function
  | Some i -> string_of_int i
  | None -> "None"

let rec ints_to_string = function
  | [] -> ""
  | x::xs -> string_of_int x ^ "; " ^ ints_to_string xs

let rec add_packagelist_to_unparse_buffer = function
  | [] -> ()
  | (Pkg(id,cur_pos,dst_pos,weight))::xs ->
	Buffer.add_string 
		unparse_buffer 
		(Printf.sprintf "		Pkg(%d,%s,%s,%s)\n"
			id
			(pos_option_to_string cur_pos)
			(pos_option_to_string dst_pos)
			(int_option_to_string weight));
	add_packagelist_to_unparse_buffer xs

let rec add_robots_to_unparse_buffer = function
  | [] -> ()
  | r::rs ->
	Buffer.add_string 
		unparse_buffer 
		(Printf.sprintf "		Robot(%d,%s,[%s])\n"
			r.robot_id
			(pos_option_to_string r.robot_position)
			(ints_to_string r.robot_packages));
	add_robots_to_unparse_buffer rs
      
let game_to_string game = 
  Buffer.reset unparse_buffer;
  Buffer.add_string unparse_buffer "GAME = {\n";
  Buffer.add_string unparse_buffer (Printf.sprintf "	id:		%d\n" game.self_id);
  let (x,y) = game.self_position in
  Buffer.add_string unparse_buffer (Printf.sprintf "	position:	(x=%d,y=%d)\n" x y);
  Buffer.add_string unparse_buffer (Printf.sprintf "	money:		%d\n" game.self_money);
  Buffer.add_string unparse_buffer (Printf.sprintf "	capacity:	%d\n" game.self_capacity);
  Buffer.add_string unparse_buffer (Printf.sprintf "   average_weight:	%d\n" game.average_weight);
  Buffer.add_string unparse_buffer "	self_packages:\n";
  add_packageidlist_to_unparse_buffer game.self_packages;
  Buffer.add_string unparse_buffer "	present_packages:\n";
  add_packageidlist_to_unparse_buffer game.present_packages;
  Buffer.add_string unparse_buffer "   game_packages:\n";
  add_packagelist_to_unparse_buffer game.game_packages;
  Buffer.add_string unparse_buffer "   game_robots:\n";
  add_robots_to_unparse_buffer game.game_robots;
  Buffer.add_string unparse_buffer "}\n";
  Buffer.contents unparse_buffer

(* UNPARSE ends *)

let mapfold (m: 'a -> 'b) (f: 'b -> 'b -> 'b) (l: 'a list) =
  match map m l with
      [] -> failwith "b"
    | [a] -> a
    | a :: (b :: r) -> fold_left f (f a b) r

let rec remove_duplicates (f: 'a -> 'a -> bool) (l: 'a list) =
  match l with
      [] -> l
    | [_] -> l
    | a :: r ->
	let s = remove_duplicates f r
	in if exists (f a) s then
	    s
	  else
	    a :: s

let distance a b =
  match (a, b) with
      ((x0, y0), (x1, y1)) -> (abs (x0 - x1)) + (abs (y0 - y1))

let package_id p =
  match p with
      Pkg (i, _, _, _) -> i

let package_destination p =
  match p with
      Pkg (_, _, Some d, _) -> d
    | _ -> failwith "c"

let package_weight p =
  match p with
      Pkg (_, _, _, Some w) -> w
    | _ -> failwith "d"

let update_position pos d = (* position -> direction -> position *)
  match pos with
      (x, y) ->
	(match d with
	     North -> (x, y + 1)
	   | South -> (x, y - 1)
	   | East -> (x + 1, y)
	   | West -> (x - 1, y))

let rec change_robot robots id f = (* robot list -> robot_id -> (robot -> robot) -> robot list *)
  match robots with
      [] -> [ f { robot_id = id; robot_position = None; robot_packages = [] } ]
    | { robot_id = robot_id } as robot :: rs ->
	if id = robot_id then
	  (f robot) :: rs
	else
	  robot :: (change_robot rs id f)

let move_rel_robot robots id direction = (* robot list -> robot_id -> direction -> robot list *)
  change_robot robots id
    (function { robot_position = pos } as robot ->
       match pos with
	   None -> robot
	 | Some p -> { robot with robot_position = Some (update_position p direction) })

let move_abs_robot robots id position = (* robot list -> robot_id -> position -> robot list *)
  change_robot robots id (function robot -> { robot with robot_position = Some position })

(* XXX *)
let lookup_robot robots id = (* robot list -> robot_id -> robot *)
  if exists (function { robot_id = robot_id } -> robot_id = id) robots then
    find (function { robot_id = robot_id } -> robot_id = id) robots
  else
    { robot_id = id; robot_position = None; robot_packages = [] }

let robot_take_package robots id package_id = (* robot list -> robot_id -> package_id -> robot list *)
  change_robot robots id
    (function { robot_packages = lst } as robot ->
       { robot with robot_packages = package_id :: lst })

let robot_drop_package robots id package_id = (* robot list -> robot_id -> package -> robot list *)
  change_robot robots id
    (function { robot_packages = lst } as robot ->
       { robot with robot_packages = filter (function pkg_id -> pkg_id <> package_id) lst })

let rec change_package packages id f = (* package list -> package_id -> (package -> package) -> package list *)
  match packages with
      [] -> [ f (Pkg (id, None, None, None)) ]
    | (Pkg (pkg_id, _, _, _)) as package :: ps ->
	if id = pkg_id then
	  (f package) :: ps
	else
	  package :: (change_package ps id f)

let package_set_position packages id position = (* package list -> package_id -> position -> package list *)
  change_package packages id
    (function Pkg (_, _, dest, wgt) -> Pkg (id, position, dest, wgt))

let package_update packages id package = (* package list -> package_id -> package -> package list *)
  change_package packages id (function _ -> package)

(* raises an exception if package not found! *)
let lookup_package packages id = (* package list -> package_id -> package *)
  try
    find (function Pkg (pkg_id, _, _, _) -> id = pkg_id) packages
  with _ ->
    printf "id = %d  len = %d\n" id (length packages); failwith "1"

let rec incorporate_reactions game reactions = (* game -> reaction list -> game *)
  match reactions with
      [] -> game
    | reaction :: rs ->
	incorporate_reactions (match reaction with
				   MovedRel (robot_id, direction) ->
				     if robot_id = game.self_id then
				       { game with self_position = update_position game.self_position direction }
				     else
				       { game with game_robots = move_rel_robot game.game_robots robot_id direction }
				 | MovedAbs (robot_id, position) ->
				     if robot_id = game.self_id then
				       { game with self_position = position }
				     else
				       { game with game_robots = move_abs_robot game.game_robots robot_id position }
				 | Picked (robot_id, package_id) ->
				     (* printf "robot %d picked %d\n" robot_id package_id; *)
				     if robot_id = game.self_id then
				       { game with self_packages = package_id :: game.self_packages;
					   game_packages = package_set_position game.game_packages package_id None }
				     else
				       { game with game_robots = robot_take_package game.game_robots robot_id package_id;
					   game_packages = package_set_position game.game_packages package_id None }
				 | Dropped (robot_id, pkg_id) ->
				     let game =
				       if robot_id = game.self_id then
					 { game with self_packages = filter ((<>) pkg_id) game.self_packages;
					     game_packages = package_set_position game.game_packages pkg_id (Some game.self_position);
					     global_strategy =
					     match game.global_strategy with
						 Some (PackageDelivery id) -> if id = pkg_id then None else game.global_strategy
					       | _ -> game.global_strategy }
				       else
					 let robot = lookup_robot game.game_robots robot_id
					 in { game with game_robots = robot_drop_package game.game_robots robot_id pkg_id;
						game_packages = package_set_position game.game_packages pkg_id robot.robot_position }
				     in let package = lookup_package game.game_packages pkg_id
				     in match package with
					 Pkg (_, Some p, Some d, _) ->
					   if d = p then
					     { game with game_packages = filter (function p -> ((package_id p) <> pkg_id)) game.game_packages }
					   else
					     game
				       | _ -> game)
	rs

let rec update_packages packages new_packages = (* package list -> package list -> package list *)
  match new_packages with
      [] -> packages
    | (Pkg (id, Some pos, Some dest, Some wgt)) as package :: ps ->
	update_packages (package_update packages id package) ps
    | _ -> failwith "e"

let remove_present_packages packages pos = (* package list -> position -> package_list *)
  filter (function Pkg (_, Some p, _, _) -> p <> pos | _ -> true) packages

let update_visited game = (* game -> unit *)
  setbit' game.self_visited (fst game.self_position) (snd game.self_position);
  iter (function { robot_position = Some (x, y) } -> setbit' game.foreign_visited x y | _ -> ())
    game.game_robots

let dodge_timeout_init () = (* unit -> int *)
  20 + (Random.int 10)

let update_dead_robots game = (* game -> robot list *)
  filter (function { robot_position = Some p } -> (p <> game.self_position) && ((getcell' game.game_board (fst p) (snd p)) <> C_Water) | _ -> true) game.game_robots

let save_game = ref None 

let update_game game packages reactions = (* game -> package list -> reaction list -> (game * package list) *)
  printf "updating with %d packages and %d reactions\n" (length packages) (length reactions);
  let game = incorporate_reactions game reactions
  in let game_packages = remove_present_packages game.game_packages game.self_position
  in let game_packages = update_packages game_packages (map (function Pkg (id, _, dest, wgt) -> Pkg (id, Some game.self_position, dest, wgt)) packages)
  in let game = { game with game_packages = game_packages; present_packages = map package_id packages }
  in let game = { game with game_robots = update_dead_robots game }
  in let game = { game with dodge_timeout =
		    if game.dodge_timeout < -20 then
		      dodge_timeout_init ()
		    else if game.dodge_timeout <= 0 then
		      game.dodge_timeout - 1
		    else
		      game.dodge_timeout }
  in (update_visited game);
    printf "dodge timeout is %d\n" game.dodge_timeout;
    print_string (game_to_string game);
    save_game := Some game;
    game

let distance_weight d = (* int -> int *)
  if d > 2 then
    1
  else if d = 2 then
    2
  else if d = 1 then
    4
  else
    10

let make_path_map game pos = (* game -> position -> unit *)
  print_string "making map\n"; flush Pervasives.stdout;
  (dijkstra game.path_map
     (fst pos) (snd pos)
     (remove_duplicates (function (p1, _) -> function (p2, _) -> p1 = p2)
	(append
	   (map (function { robot_position = Some p } -> (p, function d -> 15 * (distance_weight d)) | _ -> failwith "n")
	      (filter (function { robot_position = Some _ } -> true | _ -> false) game.game_robots))
	   (map (function Pkg (_, Some p, _, _) -> (p, function d -> -3 * (distance_weight d)) | _ -> failwith "o")
	      (filter (function Pkg (_, Some _, Some _, _) -> true | _ -> false) game.game_packages)))));
  print_string "done\n"; flush Pervasives.stdout;
  ()

let average_weight game = (* game -> int *)
  let knowns = (filter (function Pkg (_, _, _, Some w) -> true | _ -> false) game.game_packages)
  in if knowns = [] then
    1
  else
    (mapfold package_weight (+) knowns) / (length knowns)

let do_turn_and_update game turn =
  let (bid, command) = turn
  in let (packages, reactions) = do_turn game.game_server turn
  in let game = update_game game packages reactions
  in let game = { game with average_weight = average_weight game }
  in let game = { game with self_money = game.self_money - bid }
  in make_path_map game game.self_position;
    game

let path_map game = (* game -> sssps *)
  game.path_map

let direction_of_best_path game pos = (* game -> position -> direction *)
  hd (shortestpath_to (path_map game) (fst pos) (snd pos))

let length_of_best_path game pos = (* game -> position -> direction *)
  length (shortestpath_to (path_map game) (fst pos) (snd pos))

let cost_of_best_path game pos = (* game -> position -> int *)
  mindist_to (path_map game) (fst pos) (snd pos)

let cost_of_best_path_with_map map pos = (* game -> sssps -> int *)
  mindist_to map (fst pos) (snd pos)

let is_reachable game pos = (* game -> position -> bool *)
  (cost_of_best_path game pos) <> max_int

let is_reachable_in_time game pos = (* game -> position -> bool *)
  (is_reachable game pos) && ((length_of_best_path game pos) < game.self_money)

let am_i_dead game = (* game -> bool *)
  false					(* FIXME: maybe we need to be smarter *)

let nearest_robot_distance game p = (* game -> position -> int *)
  fold_left (function d -> function r ->
	       match r.robot_position with
		   None -> d
		 | Some rp ->
		     if ((distance p rp) < d) && ((length_of_best_path game rp) <= ((distance p rp) * 2)) then
		       distance p rp
		     else
		       d)
    1000000 game.game_robots

let am_i_in_danger game = (* game -> bool *)
  (nearest_robot_distance game game.self_position) < 4 (* FIXME: adaptable param *)

let am_i_in_present_danger game = (* game -> bool *)
  (nearest_robot_distance game game.self_position) < 2 (* FIXME: adaptable param *)

let am_i_in_distant_danger game = (* game -> bool *)
  (nearest_robot_distance game game.self_position) < 15 (* FIXME: adaptable param *)

let am_i_in_a_hurry game = (* game -> bool *)
  let distance = (match game.global_strategy with
		      Some (PackageSearch pos) -> length_of_best_path game pos
		    | Some (PackageDelivery pkg_id) ->
			length_of_best_path game (package_destination (lookup_package game.game_packages pkg_id))
		    | None -> 0)
  in distance * 2 > game.self_money

let water_nearby game = (* game -> bool *)
  let (x, y) = game.self_position
  in ((getcell' game.game_board (x - 1) (y - 1)) = C_Water)
     || ((getcell' game.game_board (x + 1) (y - 1)) = C_Water)
     || ((getcell' game.game_board (x - 1) (y + 1)) = C_Water)
     || ((getcell' game.game_board (x + 1) (y + 1)) = C_Water)

let best_bid game min_bid = (* game -> int -> int *)
  if ((nearest_robot_distance game game.self_position) < 2) && (water_nearby game) then
    max min_bid (game.self_money / 30)
  else
    min_bid

let cost_of_delivery game package = (* game -> package -> float *)
  (float_of_int (cost_of_best_path game (package_destination package))) /. (float_of_int (package_weight package))

let mincost (bw1, bp1) (bw2, bp2) = (* (int * a') -> (int * a') -> (int * a') *)
  if (bw1 > bw2) then
    (bw2, bp2)
  else
    (bw1, bp1)

let package_delivery_cost cost pkg = (* (position -> int) -> package -> float *)
  (float_of_int (cost (package_destination pkg))) /. (float_of_int (package_weight pkg))

let choose_package_for_delivery game packages cost = (* game -> package list -> (position -> int) -> package *)
  let (cost, package) = mapfold (function p -> (package_delivery_cost cost p, p))
			  mincost packages
  in package

let home_locations_for_search game = (* game -> (position * int * search_type) list *)
  let lst = ref []
  in for y = 1 to sizeY game.game_board do
      for x = 1 to sizeX game.game_board do
	if (getcell' game.game_board x y) = C_Homebase 
	  && not (bitsetP game.self_visited x y) 
	  && is_reachable_in_time game (x, y) then
	    lst := ((x, y), game.average_weight, if bitsetP game.foreign_visited x y then ForeignVisitedHomebase else UnvisitedHomebase) :: !lst
      done
    done;
    !lst

let consider_package game pkg = (* game -> package -> bool *)
  match pkg with
      Pkg (_, _, Some d, Some w) ->
	(w <= game.self_capacity) && (is_reachable_in_time game d)
    | _ -> true

let rec package_locations_for_search game packages = (* package list -> (position * int * search_type) list *)
  match packages with
      [] -> []
    | p :: ps ->
	if consider_package game p then
	  match p with
	      Pkg (_, Some pos, None, wgt) ->
		(pos,
		 (match wgt with
		      None -> game.average_weight
		    | Some w -> w),
		 UnsafePacket) :: (package_locations_for_search game ps)
	    | Pkg (_, Some pos, Some dest, Some w) ->
		(pos, w, SafePacket) :: (package_locations_for_search game ps)
	    | _ ->
		package_locations_for_search game ps
	else
	  package_locations_for_search game ps

let search_type_cost st = (* search_type -> int *)
  match st with
      ForeignVisitedHomebase -> 10	(* FIXME: adaptable params *)
    | UnvisitedHomebase -> 3
    | SafePacket -> 1
    | UnsafePacket -> 5

let choose_position_for_search game = (* game -> position option *)
  (* FIXME: collate positions *)
  let locations = append (home_locations_for_search game) (package_locations_for_search game game.game_packages)
  in if locations = [] then
      None
    else
      let (best_cost, best_pos) =
	mapfold (function (pos, weight, stype) ->
		   ((float_of_int ((cost_of_best_path game pos) * (search_type_cost stype))) /.
		      (float_of_int weight),
		      pos))
	  mincost locations
      in Some best_pos

let dodge_direction game = (* game -> direction option *)
  let (d, c) =
    fold_left (function (_, c1) as o1 ->
		 function (_, c2) as o2 ->
		   if c1 < c2 then o1 else o2)
      (None, max_int -1)
      (map (function d ->
	      (Some d, cost_of_best_path game (update_position game.self_position d)))
	 [North; South; East; West])
  in d

(* XXX *)
let rec package_list_diff a b = (* package_id list -> package_id list -> package_id list *)
  match a with
      [] -> []
    | id :: r ->
	if exists ((=) id) b then
	  package_list_diff r b
	else
	  id :: (package_list_diff r b)

let rec pack_packages pkgs cap = (* package list -> int -> package list *)
  match pkgs with
      [] -> []
    | Pkg (_, _, _, Some w) as p :: ps ->
	if w <= cap then
	  p :: (pack_packages ps (cap - w))
	else
	  pack_packages ps cap
    | _ ->
	failwith "r"

let packages_to_be_taken game = (* game -> package_id list *)
  let pkgs = (map (lookup_package game.game_packages) (append game.self_packages game.present_packages))
  in let pkgs = filter (consider_package game) pkgs
  in if (fold_left (function s -> function Pkg (_, _, _, Some w) -> s + w | _ -> failwith "h" ) 0 pkgs) <= game.self_capacity then
      map package_id pkgs
    else
      let main_pkg = choose_package_for_delivery game pkgs (cost_of_best_path game)
      in
	begin
	  make_path_map game (package_destination main_pkg);
	  let pkgs = sort (fun p1 p2 ->
			     let p1c = package_delivery_cost (cost_of_best_path game) p1
			     and p2c = package_delivery_cost (cost_of_best_path game) p2
			     in if p1c < p2c then
				 -1
			       else if p1c > p2c then
				 1
			       else
				 0)
		       (filter (function Pkg (id, _, _, _) -> id <> (package_id main_pkg)) pkgs)
	  in make_path_map game game.self_position;
	    map package_id (main_pkg :: pack_packages pkgs (game.self_capacity - (package_weight main_pkg)))
	end

let plan_package_exchange game = (* game -> (package_id list * package_id list) *)
  let pkgs = packages_to_be_taken game
  in let drops = package_list_diff game.self_packages pkgs
     and picks = package_list_diff pkgs game.self_packages
  in (drops, picks)

let perform_package_exchange game drops picks = (* game -> package list -> package list -> (game * package list) *)
  let game = if drops = [] then game else do_turn_and_update game (best_bid game 1, Drop drops)
  in let game = if picks = [] then game else do_turn_and_update game (best_bid game 1, Pick picks)
  in game

let walk game direction min_bid = (* game -> direction -> (game * package list) *)
  do_turn_and_update game (best_bid game min_bid, Move direction)

let nop game = (* game -> game *)
  do_turn_and_update game (1, Drop [])

let near_package_position game = (* game -> position option *)
  None					(* FIXME: implement *)

let deliverable_packages game = (* game -> package_id list *)
  map package_id (filter (function Pkg (_, _, Some d, _) -> d = game.self_position | _ -> failwith "m") (map (lookup_package game.game_packages) game.self_packages))

let can_deliver_packages game = (* game -> bool *)
  (deliverable_packages game) <> []

let find_local_strategy game = (* game -> local_strategy option *)
  print_string "finding local strategy\n";
  let dodge_is_option = (game.global_strategy <> None) && (game.dodge_timeout > 0) && ((dodge_direction game) <> None) && (am_i_in_danger game) && (not (am_i_in_a_hurry game))
  in
    begin
      if dodge_is_option then
	print_string "dodge is option\n"
      else
	print_string "dodge is not an option\n";
      if (not (am_i_in_present_danger game)) && (not dodge_is_option) && (can_deliver_packages game) then
	(print_string "dropping\n"; Some (ExchangePackages (deliverable_packages game, [])))
      else if dodge_is_option then
	match dodge_direction game with
	    Some d -> Some (Dodge d)
	  | _ -> failwith "q"
      else if game.present_packages <> [] then
	begin
	  print_string "contemplating package exchange\n";
	  match plan_package_exchange game with
	      ([], []) -> None
	    | (drops, picks) -> Some (ExchangePackages (drops, picks))
	end
      else match near_package_position game with
	  None -> None
	| Some p -> Some (LocalSearch p)
    end

let follow_local_strategy game strategy = (* game -> (game * package list) *)
  match strategy with
      Dodge d -> walk { game with dodge_timeout = game.dodge_timeout - 1 } d 1
    | LocalSearch p -> walk game (direction_of_best_path game p) 1
    | ExchangePackages (drops, picks) -> perform_package_exchange game drops picks

let find_global_strategy game = (* game -> game *)
  if game.self_packages = [] then
    let pos = choose_position_for_search game
    in match pos with
	None -> { game with global_strategy = None }
      | Some p -> {game with global_strategy = Some (PackageSearch p)}
  else
    let package = choose_package_for_delivery game (map (lookup_package game.game_packages) game.self_packages) (cost_of_best_path game)
    in {game with global_strategy = Some (PackageDelivery (package_id package))}

let print_global_strategy game = (* game -> unit *)
  match game.global_strategy with
      None -> print_string "none\n"
    | Some (PackageSearch pos) -> print_string "search at "; print_int (fst pos); print_string ","; print_int (snd pos); print_string "\n"
    | Some (PackageDelivery id) -> print_string "delivery of "; print_int id; print_string "\n"

let follow_global_strategy game = (* game -> (game * package list) *)
  print_string "following global strategy "; print_global_strategy game;
  match game.global_strategy with
      Some (PackageSearch pos) ->
	let d = direction_of_best_path game pos
	in walk game d 1
    | Some (PackageDelivery pkg_id) ->
	(match lookup_package game.game_packages pkg_id with
	     Pkg (_, _, Some pos, _) ->
	       let d = direction_of_best_path game pos
	       in walk game d 1
	   | _ -> failwith "k")
    | None -> failwith "l"

let cell_cost c = (* cell -> int *)
  match c with				(* FIXME: adaptable params *)
      C_Plain -> 3
    | C_Water -> 40
    | C_Wall -> 10
    | C_Homebase -> 1

let global_goal_reached game = (* game -> bool *)
  match game.global_strategy with
      None -> false
    | Some (PackageSearch pos) -> (game.self_position = pos) || (game.self_packages <> [])
    | Some (PackageDelivery id) -> (package_destination (lookup_package game.game_packages id)) = game.self_position

(* game -> direction option *)
(* None, wenn kein anderer robot existiert *) 
let rec hunt_down_robot_terrorist game = 
  let board = game.game_board in
  let total_pkgs = List.length game.game_packages in
  let calc_cost count_pkgs cost' =
    if total_pkgs = 0 then float_of_int cost'
    else (float_of_int cost') *. 
	 (1.0 -. ((float_of_int count_pkgs) /. (float_of_int total_pkgs))) in
  let rec evaluate_and_pick_best best = function
    | [] -> best
    | r::rs ->
        match r.robot_position with 
	  | None -> evaluate_and_pick_best best rs
	  | Some pos ->
	      let cost' = cost_of_best_path game pos in
              if cost' = max_int then 			(* robot unreachable *)
	        evaluate_and_pick_best best rs
	      else
	        let count_pkg = List.length r.robot_packages in
		let cost = calc_cost count_pkg cost' in
	        match best with
	          | None ->
		      evaluate_and_pick_best 
				(Some(cost,count_pkg,pos,r.robot_id)) rs
	          | (Some(c_old,_,_,_)) as oldval ->
		      if c_old < cost then evaluate_and_pick_best oldval rs
		      else
			evaluate_and_pick_best 
				(Some(cost,count_pkg,pos,r.robot_id)) rs in
  match evaluate_and_pick_best None (game.game_robots) with
    | None -> None
    | Some(cost,count_pkg,((pos_x,pos_y) as pos),robot_id) ->
	Printf.printf 
		"chasing robot %d holding #%d pkgs w/cost %f @ (x=%d,y=%d).\n" 
		robot_id
		count_pkg
		cost
		pos_x
		pos_y;
	flush Pervasives.stdout;
	  Some (direction_of_best_path game pos)

let rethink_global_strategy game = (* game -> game *)
  match game.global_strategy with
      None -> game
    | Some PackageSearch _ -> find_global_strategy game
    | Some PackageDelivery pkg_id ->
	let best_pkg = choose_package_for_delivery game (map (lookup_package game.game_packages) game.self_packages) (cost_of_best_path game)
	in let best_cost = package_delivery_cost (cost_of_best_path game) best_pkg
	in let current_cost = package_delivery_cost (cost_of_best_path game) (lookup_package game.game_packages pkg_id)
	in if best_cost *. 2.0 < current_cost then
	    (print_string "changing global strategy - found much better one\n";
	     find_global_strategy game)
	  else
	    game

let rec play_game game =		(* game -> Unit *)
  try
    printf "\nposition is %d, %d    %d pkgs\nglobal strategy is " (fst game.self_position) (snd game.self_position) (length game.self_packages);
    print_global_strategy game;
    flush Pervasives.stdout;
    let game =
      if global_goal_reached game then
	(print_string "\nglobal goal reached\n"; { game with global_strategy = None })
      else
	game
    in let game = rethink_global_strategy game
    in if am_i_dead game then
	()
      else
	let strategy = find_local_strategy game
	in match strategy with
	    None ->
	      printf "no local strategy (%d present)\n" (length game.present_packages);
	      let game =
		if game.global_strategy = None then
		  find_global_strategy game
		else
		  game
	      in if game.global_strategy = None then
		  let direction = hunt_down_robot_terrorist game
		  in match direction with
		      None -> play_game (nop game)
		    | Some d -> play_game (walk game d (if (nearest_robot_distance game game.self_position) < 2 then 2 else 1))
		else
		  play_game (follow_global_strategy game)
	  | Some s -> play_game (follow_local_strategy game s)
  with Failure s ->
    printf "exception %s\n" s; restart ()
    | _ -> restart ()
and restart () =
  begin
    Printf.printf "CAUGHT SOME EXCEPTION.... ";
    flush Pervasives.stdout;
    match !save_game with
      | Some g -> 
	  print_string "RESTARTING WITH SAVEGAME.\n";
	  flush Pervasives.stdout;
	  make_path_map g g.self_position;
	  play_game { g with global_strategy = None }
      | None -> 
	  print_string "FOUND NO SAVEGAME. EXITTING.\n";
	  flush Pervasives.stdout;
	  exit 12
  end


let main () =
  if Array.length Sys.argv < 3 then
    begin
      Printf.printf "usage: %s <hostname> <port>\n" Sys.argv.(0);
      exit 1
    end
  else
    let hostname = Sys.argv.(1)
    and port = int_of_string Sys.argv.(2)
    in let info = Client.initialize hostname port
    in let IStatus(id,max_carry,money,own_loc,other_locs,board,pkgs_at_curloc,s) = info
    in let game = { self_id = id;
		    self_position = own_loc;
		    self_packages = [];
		    self_money = money;
		    self_capacity = max_carry;
		    dodge_timeout = 5;	(* FIXME: adaptable param *)
		    present_packages = map package_id pkgs_at_curloc;
		    game_board = board;
		    path_map = dijkstra_init board
				 (function d -> (cell_cost C_Plain) * (distance_weight d))
				 (fun x y d -> (cell_cost (getcell' board x y)) *
				    (distance_weight d));
		    self_visited = make_empty (rawsizeX board) (rawsizeY board);
		    foreign_visited = make_empty (rawsizeX board) (rawsizeY board);
		    game_packages = map (function Pkg (id, _, dest, wgt) -> Pkg (id, Some own_loc, dest, wgt)) pkgs_at_curloc;
		    average_weight = 1;
		    game_server = s;
		    game_robots = map (function (id, pos) -> { robot_id = id; robot_position = Some pos; robot_packages = [] }) other_locs;
		    global_strategy = None }
    in
      begin
	Random.self_init ();
	make_path_map game game.self_position;
	update_visited game;
	play_game game;
	exit 0
      end

let _ = main ()
