robot:	pos.cmi pos.cmx bitmap.cmi bitmap.cmx bitmap2d.cmi bitmap2d.cmx pqueue.cmi pqueue.cmx board.cmi board.cmx client.cmi client.cmx dijkstra.cmi dijkstra.cmx robot.cmx
	ocamlopt.opt -o robot pos.cmx bitmap.cmx bitmap2d.cmx pqueue.cmx board.cmx str.cmxa unix.cmxa client.cmx dijkstra.cmx robot.cmx

clean:
	rm -f robot core *.o *.cmx *.cmi *.cmx

robot.cmx: robot.ml
	ocamlopt.opt -c robot.ml

bitmap.cmx: bitmap.ml
	ocamlopt.opt -c bitmap.ml

bitmap.cmi: bitmap.mli
	ocamlopt.opt -c bitmap.mli

bitmap2d.cmx: bitmap2d.ml
	ocamlopt.opt -c bitmap2d.ml

bitmap2d.cmi: bitmap2d.mli
	ocamlopt.opt -c bitmap2d.mli

board.cmx: board.ml
	ocamlopt.opt -c board.ml

board.cmi: board.mli
	ocamlopt.opt -c board.mli

pqueue.cmx: pqueue.ml
	ocamlopt.opt -c pqueue.ml

pqueue.cmi: pqueue.mli
	ocamlopt.opt -c pqueue.mli

client.cmx: client.ml
	ocamlopt.opt -c client.ml

client.cmi: client.mli
	ocamlopt.opt -c client.mli

dijkstra.cmx: dijkstra.ml
	ocamlopt.opt -c dijkstra.ml

dijkstra.cmi: dijkstra.mli
	ocamlopt.opt -c dijkstra.mli

pos.cmx: pos.ml
	ocamlopt.opt -c pos.ml

pos.cmi: pos.mli
	ocamlopt.opt -c pos.mli


