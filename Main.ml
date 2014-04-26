open Core.Std
open Dict
open Myset
open Prioqueue
open Graph
open Order


let server_port = 
  match Array.to_list Sys.argv with
  | [] -> failwith "Please pass in the server port number"
  | x::_ -> x

let std_response_header =
  "HTTP/1.1 200 OK\r\n" ^
    "Server: Harvard_Maps/0.0\n" ^
    "content-type: text/html; charset=utf-8\n" ^
    "Content-Language: en-us\n" ^
    "Connection: close\n\n"
;;

let maps_home_page = "./main.html" ;;

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

(** QUERIES **)

  let query_re = Str.regexp "\\?q=\\(.*\\)"
  let term_sep_re = Str.regexp "\\+"
    
  (* now returns a list rather than a query *)
  let parse_query s = 
    if Str.string_match query_re s 0 then 
      let qs = Str.matched_group 1 s in 
      let words = Str.split term_sep_re qs 
      in 
        (*parse_words *) words
    else raise (Failure "query not understood")

(* The header for search responses to clients. *)
let query_response_header =
  std_response_header ^
    "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" ^
    "<html> <head> <title>Directions and Distance</title></head>" ^
    "<body><h1>Directions and Distance:</h1><p>"
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

let do_query query_string =
  let query = parse_query query_string in
  let (start_pos, end_pos, interm) = extract_params query in
  let (x, ls) = (dijkstra firstgraph start_pos end_pos interm) in
  let response_body = (Float.to_string x) ^ (string_of_list ls) in
    query_response_header ^ response_body ^ query_response_footer

(* Given a requested path, return the corresponding local path *)
let local_path qs =
  Filename.concat root_dir qs
  
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

(* process a request -- we're expecting a GET followed by a url or a query
 * "?q=word+word".  If we find a query, then we feed it to the query parser to
 * get query abstract syntax.  Then we evaluate the query, using the index we
 * built earlier, to get a set of links.  Then we put the result in an html
 * document to send back to the client.
 *
 * If we find a url, we try to send back the correponding file.
 *
 * If we don't understand the request, then we send the default page (which is
 * just moogle.html in this directory).
 *)
let process_request client_fd request index ranks =
  (*  let _ = Printf.printf "Request: %s\n----\n" request in
      let _ = flush_all() in *)
  let is_search qs =
    let r = Str.regexp_string "?q=" in
      Str.string_match r qs 0
  in
  let is_safe s =
    (* At least check that the passed in path doesn't contain .. *)
    let r = Str.regexp_string ".." in
      try
        let _ = Str.search_forward r s 0 in
          false
      with Not_found -> true
  in
    try
      let _ = Str.search_forward http_get_re request 0 in
      let query_string = Str.matched_group 1 request in
      (*
      let _ = Printf.printf "Query string: '%s'\n\n" query_string in
      let _ = flush_all() in *)
      let response =
        if is_search query_string then
          (* print "seaching!" ;  *)
           do_query query_string index ranks
        else
          if is_safe query_string
          then read_page (local_path query_string)
          else (print "not safe!" ; std_response)
      in
      send_all client_fd response
    with _ -> send_std_response client_fd
;;

(* open a socket on the server port (specified on the command line),
 * prepare it for listening, and then loop, accepting requests and
 * sending responses.
 *)
let server (index:WordDict.dict) (ranks:RankDict.dict) =
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
    let _ = process_request client_fd request index ranks in
      Unix.close client_fd ;
      server_loop() in
    server_loop()
;;

(* On startup, create the index and then start the web server loop *)
let server index ranks =
  let _ = Printf.printf "Starting Harvard Maps on port %d.\n" server_port in
  let _ = Printf.printf "Press Ctrl-c to terminate Harvard Maps.\n" in
  let _ = flush_all () in
    server index ranks
;;

let cs124graph = NamedGraph.from_edges 
		   [("s","a", 2.); ("a","c",1.);("c","e",4.);("s","b",6.);
		    ("b","d",2.);("d","f",2.);("f","e",1.);("c","f",2.);
		    ("c","b",1.);("b","a",5.)];;

let firstgraph = NamedGraph.from_edges [
("Yenching", "J_August", 49.);
("J_August", "Leavitt", 36.);
("Leavitt", "JP_Licks", 3.);
("JP_Licks", "Gnomon", 26.);
("Felix", "Gnomon", 16.);
("Felix", "Zinnia", 49.);
("Zinnia", "STA_Travel", 528.);
("Tennis", "STA_Travel", 151.);
("Tennis", "Boloco", 20.);
("Boloco", "Spice", 138.);
("Spice", "Andover", 33.);
("Andover", "Ginos", 20.);
("Ginos", "Sandrines", 164.);
("Yenching", "Sandrines", 177.)]

module NodeHeapQueue = (BinaryHeap(PtCompare) :
                        PRIOQUEUE with type elt = PtCompare.t)

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
    if NodeHeapQueue.is_empty heap then (dist,prev)
    else 
      let ((v_node,_,v_dict), heap') = NodeHeapQueue.take heap in
      match NamedGraph.neighbors graph v_node with
      | None -> failwith "Neighborless node, impossible in our graph"
      | Some lst ->
	 let (newheap, newdist, newprev) = 
           List.fold_right 
	     lst 
	     ~f:(fun w (h,d,p) -> 
		 let (w_node,w_length) = w in
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
		    (h', d', p')
		 | Some distw, Some distv -> 
		    let distw' = distv +. w_length in
		    if distw' < distw then 
		      let h' = NodeHeapQueue.add (w_node, distw', w_dict) h in
		      let d' = DistDict.insert d (w_node, w_dict) distw' in
		      let p' = PrevDict.insert p (w_node, w_dict) 
					       (v_node, v_dict) in
		      (h', d', p')
		    else (h,d,p)
		 | _, None -> failwith("There should always be a distv"))
	     ~init: (heap',dist,prev) in 
	 helper newheap newdist newprev in
  let initial_heap = (NodeHeapQueue.add (s,0.,BoolDict.empty) 
					NodeHeapQueue.empty) in
  let initial_dist = DistDict.insert DistDict.empty (s,BoolDict.empty) 0. in
  let initial_prev = PrevDict.empty in						    
  let (final_dist,final_prev) = (helper initial_heap initial_dist
					initial_prev) in
  let final_booldict = DestinationSet.fold 
			 (fun node dict -> BoolDict.insert dict node true) 
			 BoolDict.empty interm in
  let distance = match DistDict.lookup final_dist (fin, final_booldict) with
                | None -> failwith ("Unreachable destination")
                | Some d -> d
  in let nodes = extract_path final_prev (fin, final_booldict) [fin]
  in (distance, nodes)
;;

let rec string_of_list (ls: string list) : string
  List.fold_right ~init:"" ls ~f:(fun x y -> x ^ "\n" ^ y)
;;

let build_set (lst: NamedGraph.node list) : DestinationSet.set = 
  List.fold_right lst ~f:(fun x y -> DestinationSet.insert x y)
		  ~init:DestinationSet.empty ;;

 (*let (x, ls) = (dijkstra cs124graph "s" "s" 
				    (build_set ["a";"b";"c"]));; *)

let extract_params (lst: string list) : NamedGraph.node * NamedGraph.node * DestinationSet.set =
  match lst with
  | [] |_ :: [] | _ :: _ :: [] -> failwith "not enough params"
  | _ :: hd_1 :: hd_2 :: lst' -> (hd_1, hd_2, build_set lst');;