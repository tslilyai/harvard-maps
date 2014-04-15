open Core.Std
open Dict
open Set
open Prioqueue
open Graph

let graph = NamedGraph.from_edges [("a","b", 2.); ("b","c",1.);("b","a",2.)];;

print_string (let hd::_ = (NamedGraph.nodes graph) in hd);
(*
let cmdargs = Array.to_list Sys.argv in
     Dijkstra's algorithm *)

module DistDict = Dict.Make(
  struct
    type key = string
    type value = float
    let compare = Int.compare
    let string_of_key = key
    let string_of_value = Float.to_string
  end)

module PrevDict = Dict.Make(
  struct
    type key = string
    type value = string
    let compare = String.compare
    let string_of_key = key
    let string_of_value = value
  end)

module NodeHeapQueue = (BinaryHeap(PtCompare) :
                        PRIOQUEUE with type elt = PtCompare.t)


(* store the whole modules in these variables *)
(* let heap_module = (module NodeHeapQueue : PRIOQUEUE with type elt = PtCompare.t) *)

let dijkstra (graph: NamedGraph.graph) (s: NamedGraph.node) (fin: NamedGraph.node) 
    : DistDict.dict * PrevDict.dict =
  let rec helper (H: NodeHeapQueue.queue) (dist: DistDict.dict) (prev: PrevDict.dict) 
          : DistDict.dict * PrevDict.dict =
    if NodeHeapQueue.is_empty H then (dist,prev)
    else 
      let (v, H') = NodeHeapQueue.take H in
      match NamedGraph.neighbors graph v with
      | None -> failwith "Neighborless Node"
      | Some lst ->
	 let (newheap, newdist, newprev) = 
           List.fold_right lst 
			   ~f:(fun w (h,d,p) -> 
			       let (w',length) = w in
			       match DistDict.lookup d w' with 
			       | None -> failwith "Node not inserted"
			       | Some distw -> 
				  let distw' = (DistDict.lookup dist v) + length in
				  if distw' < distw then
				    let h' = NodeHeapQueue.add (w', distw') h in
				    let d' = DistDict.insert d w' distw' in
				    let p' = PrevDict.insert p w' v in
				    (h', d', p')
				  else (h,d,p))
			   ~init: (H',dist,prev) in 
	 helper newheap newdist newprev in
		   
  let initial_heap = (NodeHeapQueue.add (s,0.) empty) in
  let initial_dist = List.fold_right (NamedGraph.nodes graph) 
				     ~f:(fun n d -> DistDict.insert d n Int.max_value) 
				     ~init:DistDict.empty in
  let initial_prev = PrevDict.empty in
						       
  helper initial_heap initial_dist initial_prev
    
   
