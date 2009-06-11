open Bitmap
open Bitmap2d
open Board
open Client
open Dijkstra

let emit s = print_string s; flush stdout

let main () =
  if Array.length Sys.argv < 3 then
  begin
    Printf.printf "usage: %s <hostname> <port>\n" Sys.argv.(0);
    exit 1
  end
  else
  begin
  
    let hostname = Sys.argv.(1)
    and port = int_of_string Sys.argv.(2) in
    let info = Client.initialize hostname port in
    begin
      let IStatus(id,max_carry,money,own_loc,other_locs,board,pkgs_at_curloc,s) = info in
      begin
	print_string "\n\n";
	Printf.printf "my robot_id: %s\n" (robotid_to_string id);
	Printf.printf "max carry capacity: %d\n" max_carry;
	Printf.printf "money: %d\n" money;
	let (x,y) = own_loc in
	Printf.printf "ownpos : (x=%d, y=%d).\n" x y;
	print_string "otherlocs: ";
	List.iter (fun (id,(x,y)) -> 
			Printf.printf "(id=%s, x=%d, y=%d); " (robotid_to_string id) x y)
		other_locs;
	print_string "board:\n";
	let tmp = Bitmap.make_empty ((sizeX board + 2) * 2) in
(*	for y = 0 to (sizeY board)+1 do
	  print_string (row_to_string' (fetch_row' board y tmp));
	  print_newline ();
	done;
*)
	print_string "pkgs_at_curloc: ";
	List.iter (fun (Pkg(pkg_id,None,Some(x,y),Some w)) -> Printf.printf "(id=%s,x=%d,y=%d, weight=%d)" (packageid_to_string pkg_id) x y w) pkgs_at_curloc;
	print_string "\n\n\n";
	Printf.printf "time %f\n" (Sys.time ());
	let dijk_res =  dijkstra board (fun _ _ _ _ -> 1) x y  in
	Printf.printf "time %f\n" (Sys.time ());
(*         dump_mindists dijk_res; *)
	print_string "\n\n";
	let dst_x = 50 and dst_y = 50 in
	let way = shortestpath_to dijk_res dst_x dst_y in
	Printf.printf "time %f\n" (Sys.time ());
	Printf.printf "costs: %d, way to x=%d, y=%d: " (mindist_to dijk_res dst_x dst_y) dst_x dst_y;
	List.iter (fun dir -> print_char (direction_to_char dir)) way;
	print_string ".\n";

        exit 0
      end
    end

(*
  Printf.printf "argv_len %d.\n" (Array.length Sys.argv);
  Printf.printf "argv[1] = %s.\n" (Sys.argv.(1));
  try
    while true do
      let str = read_line () in
      let b = Board.row_from_string str in
      emit "h1";
      Printf.printf "\nInput\t%s\nBitmap\t%s\nConv\t%s\nNatural\t%s\n"
		str 
		(Bitmap.to_string b)
		(row_to_string b)
		(row_to_string' b);
    done
  with End_of_file -> ()
*)
  end

let _ = main ()