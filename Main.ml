open Core.Std
open Dict
open Myset
open Prioqueue
open Graph
open Order

(** DJKSTRA FUNCTIONS **)


(* Insert nodes into a set *)
let build_set (lst: DataGraph.node list) : DestinationSet.set = 
  List.fold_right lst ~f:(fun x y -> DestinationSet.insert x y)
      ~init:DestinationSet.empty 
    ;;

(* Extract the start, end, and intermediate locations *)
let extract_params (lst: string list) : DataGraph.node * DataGraph.node * DestinationSet.set =
  match lst with
  | [] |_ :: [] -> failwith "not enough params"
  | hd_1 :: hd_2 :: lst' -> (hd_1, hd_2, build_set lst')
;;


(* The modified dijkstra function *)
let dijkstra (graph: DataGraph.graph) (s: DataGraph.node) (fin: DataGraph.node) 
       (interm: DestinationSet.set) 
    : (float * DataGraph.node list)=
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
      match DataGraph.neighbors graph v_node with
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
        let _ = print_string (w_node ^ "," ^ (BoolDict.string_of_dict w_dict)) in
        (h', d', p')
     | Some distw, Some distv -> 
        let distw' = distv +. w_length in
        if distw' < distw then 
          let h' = NodeHeapQueue.add (w_node, distw', w_dict) h in
          let d' = DistDict.insert d (w_node, w_dict) distw' in
          let p' = PrevDict.insert p (w_node, w_dict) 
                 (v_node, v_dict) in
          let _ = print_string (w_node ^ "," ^ (BoolDict.string_of_dict w_dict)) in
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
  let _ = print_string (DistDict.string_of_dict final_dist) in
  let distance = match DistDict.lookup final_dist (fin, final_booldict) with
                | None -> failwith ("Unreachable destination")
                | Some d -> d
  (* extract the shortest path *)
  in let nodes = extract_path final_prev (fin, final_booldict) [fin]
  in (distance, nodes)
;;

(* Tests for our modified dijkstra's - yay corner cases! *)
(*assert(dijkstra cs124graph "s" "a" DestinationSet.empty = (2.,["s";"a"]));;
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
*)
(* Print function for testing *)

 let rec print_list = function [] -> ()
   | e::l -> print_string e ; print_string " " ; print_list l;;

let (x, ls) = (dijkstra data "Yenching" "Yenching" 
				    (build_set ["Eliot";"Kirkland";"WinthropGore";"WinthropStandish"])) in
    print_list (ls); print_float x;; 


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

