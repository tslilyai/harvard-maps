open Core.Std
open Dict
open Myset
open Prioqueue
open Graph
open Order

let std_response_header =
  "HTTP/1.1 200 OK\r\n" ^
    "Server: Moogle/0.0\n" ^
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

(* Build a message that has the default Moogle home page to send
 * to clients.  The contents of the home page can be found in
 * the file moogle.html. *)
let std_response =
  read_page moogle_home_page
;;

(* The header for search responses to clients. *)
let query_response_header =
  std_response_header ^
    "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">" ^
    "<html> <head> <title>Moogle Search Results</title></head>" ^
    "<body><h1>Moogle Search Results:</h1><p><ul>"
;;

let deoptionalize opt def =
  match opt with
    | Some x -> x
    | None -> def
;;

(* Convert a set of url's to HTML to splice into the search
 * response we send to clients. *)
let html_of_urllist (links: link list) ranks : string =
  List.fold_left links
    ~f:(fun s link -> "<li>" ^
       (Printf.sprintf "%0.*f" 4
          (deoptionalize (RankDict.lookup ranks link) 0.0)) ^
       " <a href=\"" ^
       (href_of_link link) ^ "\">" ^
       (string_of_link link) ^ "</a></li>" ^ s) ~init:""
;;

(* The footer for search responses to clients. *)
let query_response_footer = "</ul><hr></body></html>"
;;

let send_std_response client_fd =
  Unix.send client_fd ~buf:std_response ~pos:0 ~len:(String.length std_response) ~mode:[]
;;

let http_get_re =
  Str.regexp_case_fold "GET[ \t]+/\\([^ \t]*\\)[ \t]+HTTP/1\\.[0-9]"
;;

let do_query query_string index ranks =
  let query = Q.parse_query query_string in
  let links = Q.eval_query index query in
  let sorted_links = sort_by_rank links ranks in
  let response_body = html_of_urllist sorted_links ranks in
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

let cmdargs = Array.to_list Sys.argv;;

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
  

let rec print_list = function [] -> ()
  | e::l -> print_string e ; print_string " " ; print_list l;;

let build_set (lst: NamedGraph.node list) : DestinationSet.set = 
  List.fold_right lst ~f:(fun x y -> DestinationSet.insert x y)
		  ~init:DestinationSet.empty ;;

 (*let (x, ls) = (dijkstra cs124graph "s" "s" 
				    (build_set ["a";"b";"c"]));; *)

let extract_params (lst: string list) : NamedGraph.node * NamedGraph.node * DestinationSet.set =
  match lst with
  | [] |_ :: [] | _ :: _ :: [] -> failwith "not enough params"
  | _ :: hd_1 :: hd_2 :: lst' -> (hd_1, hd_2, build_set lst');;

let (start_pos, end_pos, interm) = extract_params cmdargs;;

let (x, ls) = (dijkstra firstgraph start_pos end_pos interm);;  

print_list (ls);; print_float x;; 
