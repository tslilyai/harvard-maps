open Core.Std
open Dict
open Myset
open Prioqueue
open Graph
open Order

let cs124graph = NamedGraph.from_edges 
		   [("s","a", 2.); ("a","c",1.);("c","e",4.);("s","b",6.);
		    ("b","d",2.);("d","f",2.);("f","e",1.);("c","f",2.);
		    ("c","b",1.);("b","a",5.)];;

(*
let cmdargs = Array.to_list Sys.argv in
     Dijkstra's algorithm *)

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

 let build_set (lst: string list) : DestinationSet.set = 
  List.fold_right lst ~f:(fun x y -> DestinationSet.insert x y)
		  ~init:DestinationSet.empty ;;

(* let testset = DestinationSet.insert ("s") DestinationSet.empty;; *)
let (x, ls) = (dijkstra cs124graph "s" "s" 
				    (build_set ["s";"a";"b";"c";"d";"e";"f"]));;

print_list (ls); print_float x;;
