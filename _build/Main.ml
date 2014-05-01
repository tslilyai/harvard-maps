open Core.Std
open Dict
open Myset
open Prioqueue
open Graph
open Order

(** DJKSTRA FUNCTIONS **)


(* Insert nodes into a set *)
let build_set (lst: NamedGraph.node list) : DestinationSet.set = 
  List.fold_right lst ~f:(fun x y -> DestinationSet.insert x y)
      ~init:DestinationSet.empty 
    ;;

(* Extract the start, end, and intermediate locations *)
let extract_params (lst: string list) : NamedGraph.node * NamedGraph.node * DestinationSet.set =
  match lst with
  | [] |_ :: [] -> failwith "not enough params"
  | hd_1 :: hd_2 :: lst' -> (hd_1, hd_2, build_set lst')
;;

(* Graph for testing from Jelani's lecture notes *)
let cs124graph = NamedGraph.from_edges 
 		   [("s","a", 2.); ("a","c",1.);("c","e",4.);("s","b",6.);
 		    ("b","d",2.);("d","f",2.);("f","e",1.);("c","f",2.);
		    ("c","b",1.);("b","a",5.)];;

(* Create our graph *)
let data = NamedGraph.from_edges [
("Yenching", "J_August", 49.);
("J_August", "Leavitt", 36.);
("Leavitt", "JP_Licks", 3.);
("JP_Licks", "Gnomon", 26.);
("Felix", "Gnomon", 16.);
("Felix", "Zinnia", 49.);
("Zinnia", "AdamsClaverly", 528.);
("Tennis", "AdamsClaverly", 151.);
("Tennis", "Boloco", 20.);
("Boloco", "Spice", 138.);
("Spice", "Andover", 33.);
("Andover", "Ginos", 20.);
("Ginos", "Sandrines", 164.);
("Yenching", "Sandrines", 177.);
("University_hall","Weld", 184.);
("Weld","Grays", 112.);
("Grays","Boylston", 121.);
("Boylston","Wigglesworth",200.);
("Wigglesworth","Lamont",479.);
("Lamont","Widener",456.);
("Wigglesworth","Widener",420.);
("Widener","Emerson",230.);
("Emerson","Sever",217.);
("Widener","Sever",249.);
("Weld","Widener", 384.);
("Widener","Mem_church",400.);
("Grays","Matthews", 190.);
("Grays","Mass_hall", 240.);
("Grays","University_hall",285.);
("Mass_hall","University_hall", 325.);
("Grays","Straus", 92.);
("Straus","Mass_hall", 246.);
("Lamont","Emerson",482.);
("Mem_church","Canaday", 226.);
("Thayer","Holworthy",161.);
("Thayer","Canaday",180.);
("Holworthy","Stoughton",157.);
("Stoughton","Hollis",112.);
("Stoughton","Mower",144.);
("Mower","Lionel",171.);
("Lionel","Hollis",144.);
("Mass_hall","Hollis",295.);
("Yenching","Wigglesworth",240.);
("Pfoho","Cabot", 295.);
("Cabot","Soch",400.);
("Currier","Cabot",325.);
("Currier","Pfoho",100.);
("Soch","Mass_hall",3168.);
("AdamsDhall","Lampoon",203.);
("AdamsClaverly","Lampoon",117.);
("Harvard_Bookstore","Crimson",177.);
("Harvard_Bookstore","Wigglesworth",144.);
("Crimson","AdamsDhall",92.);
("Lampoon","Quincy",161.);
("Quincy","Lev_McKinlock",476.);
("LevMcKinlock","Lev_Towers",262.);
("LevTowers","Dunster",312.);
("Dunster","Mather",187.);
("Lev_McKinlock","WinthropGore",322.);
("Quincy","Lowell",184.);
("WinthropGore","WinthropStandish",377.);
("WinthropStandish","Eliot",187.);
("Eliot","Kirkland",128.);
("Kirkland","MAC",299.);
("MAC","Lowell",217.);
("MAC","UHS",374.);
("UHS","Finale",210.);
("Finale","Smith",226.);
("Smith","T_Station",118.);
("Smith","Yenching",141.);
("Spice","UHS",95.);
]

;;

(* Inserts location-coordinate key-value pairs into the dictionary *)
let insert_locations (ls : (string*string) list) : LocationDict.dict =
  List.fold_left ls ~f:(fun d (k, v) -> LocationDict.insert d k v) ~init:LocationDict.empty
;;

(* Insert coordinates of each location into LocationDict *)
let location_pts = insert_locations [
("Yenching", "42.372976,-71.117853");
("J_August", "42.372872,-71.117717");
("Boloco", "42.372038,-71.11829");
("JP_Licks","42.372922,-71.117552");
("Leavitt","42.372912,-71.117641");
("Gnomon","42.372888,-71.117458");
("Zinnia","42.372823,-71.11727");
("Spice","42.372181,-71.118386");
("Andover","42.372264,-71.11833");
("Ginos","42.372319,-71.118263");
("Tennis","42.37199,-71.118287");
("Sandrines","42.372731,-71.118048");
("Felix","42.372882,-71.117426");
("Wigglesworth","42.37309,-71.117164");
("Lamont","42.372722,-71.115501");
("Widener","42.373284,-71.11652");
("Boylston","42.373371,-71.117319");
("Grays","42.373665,-71.117797");
("Matthews","42.374097,-71.11814");
("Emerson","42.373891,-71.115254");
("Sever","42.374374,-71.115522");
("Mem_church","42.374941,-71.115924");
("Thayer","42.37506,-71.116734");
("Mass_hall","42.374477,-71.118328");
("Straus", "42.37418,-71.118607");
("Hollis","42.375004,-71.117845");
("Stoughton","42.375369,-71.117743");
("Mower","42.375464,-71.118247");
("Lionel","42.375155,-71.118354");
("Canaday","42.375341,-71.116037");
("University_hall","42.374497,-71.117078");
("Weld","42.373994,-71.117126");
("Holworthy","42.375519,-71.117115");
("Pfoho","42.382125,-71.124911");
("Cabot","42.381499,-71.124245");
("Currier","42.381769,-71.125629");
("Soch","42.380889,-71.125147");
("Crimson","42.372151,-71.116541");
("AdamsDhall", "42.371925,-71.116649");
("AdamsClaverly","42.371915,-71.117734");
("Lampoon","42.371572,-71.117196");
("Lowell","42.370986,-71.117807");
("Quincy","42.370998,-71.117169");
("Lev_McKinlock","42.370102,-71.117587");
("Lev_Towers","42.369666,-71.116412");
("Dunster","42.368774,-71.115919");
("Mather","42.368659,-71.115205");
("WinthropGore","42.370391,-71.118639");
("WinthropStandish","42.370451,-71.119862");
("Eliot","42.370407,-71.120956");
("Kirkland","42.370831,-71.120452");
("MAC","42.37139,-71.119465");
("UHS","42.372171,-71.118666");
("Finale","42.372492,-71.119079");
("Smith","42.372979,-71.11844");
("T_Station","42.37328,-71.118907");
("Harvard_Bookstore","42.372539,-71.116353")
]
;;

(* Initialize our min-priority queue *)
module NodeHeapQueue = (BinaryHeap(PtCompare) :
                        PRIOQUEUE with type elt = PtCompare.t)
;;

(* The modified dijkstra function *)
let dijkstra (graph: NamedGraph.graph) (s: NamedGraph.node) (fin: NamedGraph.node) 
       (interm: DestinationSet.set) 
    : (float * NamedGraph.node list)=
  let rec extract_path prev_dict node path = 
    match PrevDict.lookup prev_dict node with
                  | None -> path
                  | Some (str,dict) -> extract_path prev_dict (str,dict) (str::path)
  in
  let rec helper (heap: NodeHeapQueue.queue) (dist: DistDict.dict) 
     (prev: PrevDict.dict) 
          : DistDict.dict * PrevDict.dict =
    (* check if the heap is empty *)
    if NodeHeapQueue.is_empty heap then (dist,prev)
    else 
    (* take the node with the minimum distance from the heap *)
      let ((v_node,_,v_dict), heap') = NodeHeapQueue.take heap in
      (* traverse all edges coming from v_node *)
      match NamedGraph.neighbors graph v_node with
      | None -> failwith "Neighborless node, impossible in our graph"
      | Some lst ->
   let (newheap, newdist, newprev) = 
           List.fold_right 
       lst 
       ~f:(fun w (h,d,p) -> 
     let (w_node,w_length) = w in
     (* check if the neighbor is one of the intermediate nodes *)
     let w_dict = 
       if DestinationSet.member interm w_node
       then BoolDict.insert v_dict w_node true
       else v_dict in
     match (DistDict.lookup d (w_node, w_dict)), 
           (DistDict.lookup dist (v_node, v_dict)) with 
     | None, Some distv -> 
        let distw' = distv +. w_length in
        let h' = NodeHeapQueue.add (w_node, distw', w_dict) h in
        let d' = DistDict.insert d (w_node, w_dict) distw' in
        let p' = PrevDict.insert p (w_node, w_dict) 
               (v_node, v_dict) in
        (*let _ = print_string (w_node ^ "," ^ (BoolDict.string_of_dict w_dict)) in *)
        (h', d', p')
     | Some distw, Some distv -> 
        let distw' = distv +. w_length in
        if distw' < distw then 
          let h' = NodeHeapQueue.add (w_node, distw', w_dict) h in
          let d' = DistDict.insert d (w_node, w_dict) distw' in
          let p' = PrevDict.insert p (w_node, w_dict) 
                 (v_node, v_dict) in
          (*let _ = print_string (w_node ^ "," ^ (BoolDict.string_of_dict w_dict)) in *)
          (h', d', p')
        else (h,d,p)
     | _, None -> failwith("There should always be a distv"))
       ~init: (heap',dist,prev) in 
   (*print_string (DistDict.string_of_dict newdist);  *) 
   helper newheap newdist newprev in
  (* check if the start node is one of the intermediate nodes *)
  let s_dict = 
       if DestinationSet.member interm s
       then BoolDict.insert BoolDict.empty s true
       else BoolDict.empty in 
  (* initial heap, dist, and prev *)
  let initial_heap = (NodeHeapQueue.add (s,0.,s_dict  (*BoolDict.empty*)) 
          NodeHeapQueue.empty) in
  let initial_dist = DistDict.insert DistDict.empty (s,s_dict (*BoolDict.empty*)) 0. in
  let initial_prev = PrevDict.empty in
  (* run the helper function on our initial values *)                
  let (final_dist,final_prev) = (helper initial_heap initial_dist
          initial_prev) in
  (* add all the intermediate destinations as keys and "visited" bools as values *)
  let final_booldict = DestinationSet.fold 
       (fun node dict -> BoolDict.insert dict node true) 
       BoolDict.empty interm in
  (* get the shortest distance of our path *)
  (*let _ = print_string (DistDict.string_of_dict final_dist) in *)
  let distance = match DistDict.lookup final_dist (fin, final_booldict) with
                | None -> failwith ("Unreachable destination")
                | Some d -> d
  (* extract the shortest path *)
  in let nodes = extract_path final_prev (fin, final_booldict) [fin]
  in (distance, nodes)
;;

(* Tests for our modified dijkstra's - yay corner cases! *)
assert(dijkstra cs124graph "s" "a" DestinationSet.empty = (2.,["s";"a"]));;
assert(dijkstra cs124graph "s" "s" DestinationSet.empty = (0.,["s"]));;
assert(dijkstra cs124graph "s" "f" DestinationSet.empty = (5.,["s";"a";"c";"f"]));;
assert(dijkstra cs124graph "s" "s" (build_set ["s";"a";"b";"c";"d";"e";"f"]) = (15., ["s";"a";"c";"b";"d";"f";"e";"f";"c";"a";"s"]));; 
assert(dijkstra cs124graph "s" "a" (build_set ["s";"a";"b";"c";"d";"e";"f"]) = (13., ["s";"a";"c";"b";"d";"f";"e";"f";"c";"a"]));; 
assert(dijkstra cs124graph "c" "f" (build_set ["s";"a";"b";"c";"d";"e";"f"]) = (15., ["c";"b";"c";"a";"c";"b";"c";"a";"c";"a";"c";"b";"d";"f"]));;
assert(dijkstra cs124graph "s" "e" (build_set ["s"]) = (6., ["s";"a";"c";"f";"e"]));;  
assert(dijkstra cs124graph "s" "e" (build_set ["e"]) = (6., ["s";"a";"c";"f";"e"]));;  
assert(dijkstra cs124graph "s" "s" (build_set ["s"]) = (0., ["s"]));;
assert(dijkstra cs124graph "s" "e" (build_set ["b"]) = (8., ["s";"a";"c";"b";"c";"f";"e"]));;
assert(dijkstra cs124graph "s" "e" (build_set ["b";"c"]) = (8., ["s";"a";"c";"b";"c";"f";"e"]));;

(* Print function for testing *)

(* let rec print_list = function [] -> ()
   | e::l -> print_string e ; print_string " " ; print_list l;;

let (x, ls) = (dijkstra data "Yenching" "Yenching" 
				    (build_set ["Eliot";"Kirkland";"WinthropGore";"WinthropStandish"])) in
    print_list (ls); print_float x;; 
*)

(* Get the server port number, usally 8080 *)
let server_port =
  let args = Sys.argv in
    try
      let port = int_of_string(Array.get args 1) in
        port
    with
        exn -> (Printf.printf
                  "usage: %s <port>\n"
                  (Array.get args 0) ;
                exit 1)

let std_response_header =
  "HTTP/1.1 200 OK\r\n" ^
    "Server: Harvard_Maps/0.0\n" ^
    "content-type: text/html; charset=utf-8\n" ^
    "Content-Language: en-us\n" ^
    "Connection: close\n\n"
;;

let maps_home_page = "./Main.html" ;;

(* read in all the lines from a file and concatenate them into
 * a big string. *)
let rec input_lines inchan lines =
  try
    input_lines inchan ((input_line inchan)::lines)
  with End_of_file -> List.rev lines
;;

(* Read the contents of a webpage.
 * page : string, a filename*)
let read_page page =
  let _ = Printf.printf "reading '%s'\n" page in
  let _ = flush_all() in
  let ch = open_in page in
  let lines = input_lines ch [] in
  let resp = String.concat ~sep:"" lines in
    In_channel.close ch ; resp
;;

(* Build a message that has the default Main home page to send
 * to clients.  The contents of the home page can be found in
 * the file Main.html. *)
let std_response =
  read_page maps_home_page
;;

(** QUERY FUNCTIONS **)

  let query_re_begin = Str.regexp "\\?begin=\\(.*\\)"
;;
  let term_sep_re_end = Str.regexp "\\&end="
;;
  let term_sep_re_interms = Str.regexp "\\&interms="
;;    


  (* Tturns a list of the provided arguments *)
  let parse_query s = 
    if Str.string_match query_re_begin s 0 then 
      let qs = Str.matched_group 1 s in 
      match Str.split term_sep_re_end qs with
      | start::rest::[] -> 
   let args = Str.split term_sep_re_interms rest in 
         start::args
      | _ -> raise (Failure "No start or end specified!")
    else raise (Failure "query not understood")
;;

(* The header for search responses to clients. *)
let query_response_header =
  std_response_header ^
    "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" ^
    "<html> <head> <title align=\"center\">Directions and Distance</title></head>" ^
    "<body align=\"center\"><h1>Directions and Distance</h1><p>"
;;

(* The footer for search responses to clients. *)
let query_response_footer = "<hr></body></html>"
;;

let send_std_response client_fd =
  Unix.send client_fd ~buf:std_response ~pos:0 ~len:(String.length std_response) ~mode:[]
;;

let http_get_re =
  Str.regexp_case_fold "GET[ \t]+/\\([^ \t]*\\)[ \t]+HTTP/1\\.[0-9]"
;;

(* Markers for the start and end points on the map *)
let string_of_markers start_pos end_pos = 
  let start_loc = (
    match LocationDict.lookup location_pts start_pos with
    | None -> raise (Failure "Location not present")
    | Some x -> x )
  in
  let end_loc = (
    match LocationDict.lookup location_pts end_pos with
    |None -> raise (Failure "Location not present")
    |Some x -> x )
  in
  ("&markers=size:mid%7Clabel:S%7C"  ^ start_loc ^ "&markers=size:mid%7Clabel:E%7C" ^ end_loc)
;;

(* Markers for the intermediate locations on the map *)
let string_of_interms ls = 
  let rec interms_string interms = 
    match interms with
    | [] -> ""
    | hd::tl -> (match LocationDict.lookup location_pts hd with
                 | None -> raise (Failure "Location not present")
                 | Some x -> "%7C" ^ x ^ (interms_string tl))
  in "&markers=size:mid%7Ccolor:green%7Clabel:.%7C" ^ (interms_string ls)
;;

(* Turn a list into a string *)
let string_of_list (ls: string list) (interms: DestinationSet.set): string =
  "<ol>" ^ List.fold_right ~init:"</ol>" ls ~f:(fun x y -> 
                                                    if DestinationSet.member interms x then
                                    "<li>" ^ "<font color="^"green"^">"^x ^"</font>"^ y
                                    else "<li>" ^ x ^ y)

(* Here we can trace out the path itself *) 
let string_of_path node_list = 
  let rec node_string nodes = 
    match nodes with
    | [] -> ""
    | hd::tl -> (match LocationDict.lookup location_pts hd with
                 | None -> raise (Failure "Location not present")
                 | Some x -> "%7C" ^ x ^ (node_string tl))
  in "path=weight:3%7Ccolor:red" ^ (node_string node_list)
;;

(* Processes the query, returns the output to be displayed *)
let do_query query_string =
  let query = parse_query query_string in
  let (start_pos, end_pos, interm) = extract_params query in
  let (x, ls) = (dijkstra data start_pos end_pos interm) in
  let distance = (Float.to_string x) ^ "\n" in
  let destinations = (string_of_list ls interm) in
  let start_end_string = (string_of_markers start_pos end_pos) in
  let interms_string = (string_of_interms query) in
  let path_string = (string_of_path ls) in
    query_response_header ^ "Distance: " ^ distance ^ "feet" ^ "<br><table align=\"center\" cellpadding=\"10\"><tr><td valign=\"top\">" 
    ^ "Directions: " ^ destinations ^ "</td><td> " ^ 
    "<img src=" ^ "\"http://maps.googleapis.com/maps/api/staticmap?" ^ path_string ^ "&size=640x640&sensor=false" ^ start_end_string ^ interms_string ^ "\"></img></td></tr></table>"
    ^ query_response_footer
;;  

let send_all fd buf =
  let rec more st size =
    let res = Unix.send fd ~buf:buf ~pos:st ~len:size ~mode:[] in
    if res < size then
      more (st + res) (size - res)
    else ()
  in
  let size = String.length buf in
  let _ = more 0 size in size
;;

(* process a request -- If we find a query, then we feed it to the query parser to
 * get query abstract syntax.  Then we evaluate the query, and we put the result in an html
 * document to send back to the client.
 *
 *)
let process_request client_fd request =
    try
      let _ = Str.search_forward http_get_re request 0 in
      let query_string = Str.matched_group 1 request in

      let response =
           do_query query_string
      in
      send_all client_fd response
    with _ ->  send_std_response client_fd 
;;

(* open a socket on the server port (specified on the command line),
 * prepare it for listening, and then loop, accepting requests and
 * sending responses.
 *)
let server () =
  let fd = Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_STREAM ~protocol:0 in
  let sock_addr = Unix.ADDR_INET (Unix.Inet_addr.bind_any, server_port) in
  let _ = Unix.setsockopt fd Unix.SO_REUSEADDR true in
  let _ = Unix.bind fd ~addr:sock_addr in
  let _ = Unix.listen fd ~max:5 in  (* at most 5 queued requests *)
  let rec server_loop () =
    (* allow a client to connect *)
    let (client_fd, _) = Unix.accept fd in
    let buf = String.create 4096 in
    let len = Unix.recv client_fd ~buf:buf ~pos:0 ~len:(String.length buf) ~mode:[] in
    let request = String.sub buf ~pos:0 ~len:len in
    let _ = process_request client_fd request in
      Unix.close client_fd ;
      server_loop() in
    server_loop()
;;

(* On startup, create the index and then start the web server loop *)
let server () =
  let _ = Printf.printf "Starting Harvard Maps on port %d.\n" server_port in
  let _ = Printf.printf "Press Ctrl-c to terminate Harvard Maps.\n" in
  let _ = flush_all () in
    server ()
;;
    
let main () =
  (* Want different random numbers every time. *)
  let _ = Random.self_init () in
  let _ = flush_all () in
    server () 
  ;; 

main ();;    

