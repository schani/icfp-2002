open Pos
open Board
open Client

type sssps

val dijkstra_init : board -> (int -> int) -> (int -> int -> int -> int) -> sssps

val dijkstra : 
	sssps -> 
	int -> 
	int -> 
	((int * int) * (int -> int)) list -> 
	unit

val mindist_to : sssps -> int -> int -> int
val shortestpath_to : sssps -> int -> int -> direction list

val dump_mindists : sssps -> unit
