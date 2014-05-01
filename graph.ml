open Core.Std

module type NODE =
sig
  type node

  (* Require that nodes be comparable for efficiency. *)
  val compare : node -> node -> Ordering.t
  val string_of_node : node -> string
  val gen : unit -> node
end

(* A signature for undirected graphs with weighted edges *)
module type GRAPH =
sig
  module N : NODE
  type node = N.node
  type graph

  val empty : graph

  val nodes : graph -> node list

  val is_empty : graph -> bool

  val add_node : graph -> node -> graph

  (* Adds the nodes if they aren't already present. Adds both the forward
     and back edge. 
  *)
  val add_edge : graph -> node -> node -> float  -> graph

  (* Return None if node isn't in the graph *)
  val neighbors : graph -> node -> (node * float) list option

  (* Return None if node isn't in the graph *)
  val outgoing_edges : graph -> node -> (node * node * float) list option

  val has_node : graph -> node -> bool

  val string_of_graph : graph -> string
end

module Graph(NA: NODE) : (GRAPH with module N = NA) =
struct
  open Order;;
  module N = NA
  type node = N.node

  (* A graph represetned as an edge dictionary:
     dictionary: node -> neighbor set of tuples of (node, weight)
     Every node in the graph must be a key in the dictionary.
  *)

 (* Set containing all neighbors and their edge weights *)
  module NeighborSet = Myset.Make(
     struct
        type t = (node * float)
        let compare x y = let (n1, _), (n2, _) = x, y in N.compare n1 n2
        let string_of_t x = let (n, _) = x in N.string_of_node n
        let gen ()= (N.gen (), 3.)
        let gen_random () = (N.gen (), (Random.float 100.))
        let gen_gt (_, _) () = (N.gen (), 3.)
      end)

 (* Dictionary mapping a node to its neighbors *)
  module EdgeDict = Dict.Make(
    struct
      type key = node
      type value = NeighborSet.set
      let compare = N.compare
      let string_of_key = N.string_of_node
      let string_of_value ns = NeighborSet.string_of_set ns
      let gen_key = N.gen
      let gen_key_random = N.gen
      let gen_key_gt _ () = N.gen ()
      let gen_value () = NeighborSet.empty
      let gen_pair () = (gen_key(),gen_value())
    end)

  module IntNode = Dict.Make(
    struct
      type key = int
      type value = node
      let compare = int_compare
      let string_of_key = string_of_int
      let string_of_value = N.string_of_node
      let gen_key () = 0
      let gen_key_random () = 0
      let gen_key_gt _ () = 1
      let gen_value = N.gen
      let gen_pair () = (gen_key(),gen_value())
    end)

 type graph = { edges : EdgeDict.dict ;
                num_nodes : int ;
                index_to_node_map : IntNode.dict }

 let empty : graph = { edges = EdgeDict.empty;
                       num_nodes = 0;
                       index_to_node_map = IntNode.empty }

 let add_node g n =
   if EdgeDict.member g.edges n then g
   else
     { edges = EdgeDict.insert g.edges n (NeighborSet.empty) ;
       num_nodes = g.num_nodes + 1 ;
       index_to_node_map =
         IntNode.insert g.index_to_node_map g.num_nodes n }

  let nodes g =
    EdgeDict.fold (fun k _ r -> k :: r) [] g.edges

  let is_empty g = (g.num_nodes = 0)

  (* Adds the nodes if they aren't already present. *)
  let add_edge g src dst weight =
    let new_neighbors = match EdgeDict.lookup g.edges src with
      | None -> NeighborSet.insert (dst, weight) NeighborSet.empty
      | Some s -> NeighborSet.insert (dst, weight) s
    in
    let new_neighbors' = match EdgeDict.lookup g.edges dst with
      | None -> NeighborSet.insert (src, weight) NeighborSet.empty
      | Some s -> NeighborSet.insert (src, weight) s
    in
      (* ensure both src and dst in the graph before adding edge *)
    let g' = (add_node (add_node g src) dst) in
      {edges = (let edges1 = EdgeDict.insert g'.edges src new_neighbors in 
		EdgeDict.insert edges1 dst new_neighbors');
       num_nodes = g'.num_nodes;
       index_to_node_map = g'.index_to_node_map}

  let neighbors g n : (node * float) list option =
    match EdgeDict.lookup g.edges n with
      | None -> None
      | Some s -> Some (NeighborSet.fold (fun neigh r -> neigh :: r) [] s)

  let outgoing_edges g src : (node * node * float) list option =
    match EdgeDict.lookup g.edges src with
      | None -> None
      | Some s -> Some (NeighborSet.fold (fun (dst, weight) r ->
                                             (src, dst, weight) :: r) [] s)
  let has_node g n =
    match EdgeDict.lookup g.edges n with
      | None -> false
      | _ -> true


  let string_of_graph g =
    "Graph: " ^ (EdgeDict.string_of_dict g.edges)
end

(* Graph that holds all the data *)
module DataGraph =
struct
  include(Graph(struct
                  type node = string
                  let compare = Order.string_compare
                  let string_of_node = fun x -> x
                  let gen () = ""
                end))
  let from_edges (es: (string * string * float) list) : graph =
    List.fold_left es ~f:(fun g (src, dst, weight) -> add_edge g src dst weight) ~init:empty
end

(* Tests *)
module TestGraph =
struct
  module G = DataGraph

  let g = G.add_edge G.empty "a" "b" 3.;;
  let g2 = G.add_edge g "a" "c" 3.;;

  let deopt_len lo =
    match lo with
      | None -> 0
      | Some xs -> List.length xs;;

  let deopt_lst lo =
    match lo with
      | None -> []
      | Some xs -> xs;;

  let deopt_node no =
    match no with
      | None -> "None"
      | Some n -> n;;

  let _ = (
    assert (G.has_node g "a");
    assert (G.has_node g "b");
    assert (G.has_node g "c" = false);
    assert (G.has_node g2 "c");
    assert (G.has_node g2 "d" = false);

    assert (List.length (G.nodes G.empty) = 0) ;
    assert (List.length (G.nodes (G.add_node G.empty "a")) = 1) ;

    assert (List.length (G.nodes g) = 2) ;

    assert (List.length (G.nodes g2) = 3) ;

    assert (deopt_len (G.outgoing_edges g2 "a") = 2) ;
    assert (G.outgoing_edges g2 "d" = None) ;

    assert (deopt_len (G.neighbors g2 "a") = 2) ;
    assert (G.neighbors g2 "d" = None);
)

end


(* Graph for testing from Jelani's lecture notes *)
let cs124graph = DataGraph.from_edges 
       [("s","a", 2.); ("a","c",1.);("c","e",4.);("s","b",6.);
        ("b","d",2.);("d","f",2.);("f","e",1.);("c","f",2.);
        ("c","b",1.);("b","a",5.)];;

(* Create our graph *)
let data = DataGraph.from_edges [
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
