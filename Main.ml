open Core.Std
open Dict
open Myset
open Prioqueue
open Graph
open Order

(** DJKSTRA FUNCTIONS **)

let string_of_list (ls: string list) : string =
  "<ol>" ^ List.fold_right ~init:"</ol>" ls ~f:(fun x y -> "<li>" ^ x ^ y)
;;

let build_set (lst: NamedGraph.node list) : DestinationSet.set = 
  List.fold_right lst ~f:(fun x y -> DestinationSet.insert x y)
      ~init:DestinationSet.empty ;;

let extract_params (lst: string list) : NamedGraph.node * NamedGraph.node * DestinationSet.set =
  match lst with
  | [] |_ :: [] -> failwith "not enough params"
  | hd_1 :: hd_2 :: lst' -> (hd_1, hd_2, build_set lst');;

let data = NamedGraph.from_edges [
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

let location_pts = LocationDict.insert_locations [
("Yenching", "42.372976, -71.117853")
("J_August", "42.372872, -71.117717")
]

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
    (* check if the heap is empty *)
    if NodeHeapQueue.is_empty heap then (dist,prev)
    else 
    (* take the node with the minimum distance from the heap*)
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
  (* check if the start node is one of the intermediate nodes *)
  let s_dict = 
       if DestinationSet.member interm s
       then BoolDict.insert BoolDict.empty s true
       else BoolDict.empty in
  let initial_heap = (NodeHeapQueue.add (s,0.,s_dict) 
          NodeHeapQueue.empty) in
  let initial_dist = DistDict.insert DistDict.empty (s,s_dict) 0. in
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

(*let server_port = 
  match Array.to_list Sys.argv with
  | [] -> failwith "Please pass in the server port number"
  | _::x::_ -> int_of_string x *)
  
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
  let term_sep_re_interms = Str.regexp "\\&interms="
;;    

  (* now returns a list rather than a query *)
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

let string_of_markers ls = 
  (match ls with
   | start::fin::tl -> (
     let start_loc = (
       match LocationDict.lookup start with
       | None -> Failure "Location not present"
       | Some x -> x )
     in
     let end_loc = (
       match LocationDict.lookup fin with
       |None -> Failure "Location not present"
       |Some x -> x )
     in
     ("&markers=size:mid%7Ccolor:red%7C" ^ start_loc ^ "%7C" ^ end_loc)))
  | _ -> (Failure "Not enough arguments specified")

let string_of_interms ls = 
  let rec interms_string interms = 
    match interms with
    | [] -> ""
    | hd::tl -> (match LocationDict.lookup hd with
                 | None -> Failure "Location not present"
                 | Some x -> x ^ "%7C" ^ (interms_string tl))
  in "&path=weight:5%7Ccolor:red%7C" ^ (interms_string ls)

let do_query query_string =
  let query = parse_query query_string in
  let (start_pos, end_pos, interm) = extract_params query in
  let (x, ls) = (dijkstra data start_pos end_pos interm) in
  let distance = (Float.to_string x) ^ "\n" in
  let destinations = (string_of_list ls) in
  let start_end_string = (string_of_markers ls) in
  let interms_string = (string_of_interms ls)
    query_response_header ^ "Distance: " ^ distance ^ "feet" ^ "<br> <br>" 
    ^ "Directions: " ^ destinations ^ 
    "<img src=" ^ "\"http://maps.googleapis.com/maps/api/staticmap?center=42.3723504,-71.118163&zoom=17&size=640x640&sensor=false" ^ start_end_string ^ interms_string ^ "\"></img>"
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

(* process a request -- we're expecting a GET followed by a url or a query
 * "?begin=word+word".  If we find a query, then we feed it to the query parser to
 * get query abstract syntax.  Then we evaluate the query, using the index we
 * built earlier, to get a set of links.  Then we put the result in an html
 * document to send back to the client.
 *
 *)
let process_request client_fd request =
  (*  let _ = Printf.printf "Request: %s\n----\n" requestin
      let _ = flush_all() in *)
    try
      let _ = Str.search_forward http_get_re request 0 in
      let query_string = Str.matched_group 1 request in
      
      (*let _ = Printf.printf "Query string: '%s'\n\n" query_string in
      let _ = flush_all() in *)
      let response =
           (*Printf.printf "seaching!" ;  *)
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
    (* Construct the index to pass to the server *)
  let _ = flush_all () in
    server () 
  ;; 

main ();;    

