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
        (graph: NamedGraph.graph) : DistDict.dict * PrevDict.dict =
            match H with
            | empty
    
    in
    let initial_heap = (NodeHeapQueue.add (s,0.) empty))
    let initial_dist = (* insert Int.max_value for all the nodes *)
    let initial_prev = (* can leave empty? *)

    helper initial_heap initial_dist initial_prev
    
   
