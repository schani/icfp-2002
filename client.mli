
open Board
open Unix

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

(* (id,max_carrying_capacity,money,ownpos,otherpos,board,packages_at_ownpos,fildes) *)
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

val direction_to_char : direction -> char

val robotid_to_string : robot_id -> string
val packageid_to_string : package_id -> string

val robotid_of_string : string -> robot_id
val packageid_of_string : string -> package_id

val initialize : string -> int -> initstatus

(** [Client.initialize ()] lets the client join the game.
    It establishes the setup of the communication line and
    manages the initial transaction with the server.
	1) Send the string Player
	2) Wait for the board
	3) Wait for the player's configuation
	4) Wait for the location of all players

	5) Wait for a list of packages at the player's current configuration.
 **)

type command = 
  | Move of direction
  | Pick of package_id list
  | Drop of package_id list

type reaction = 
  | MovedRel of robot_id * direction
  | MovedAbs of robot_id * position
  | Picked of robot_id * package_id
  | Dropped of robot_id * package_id

val do_turn : (in_channel * out_channel) -> (int * command) -> (package list * reaction list)

