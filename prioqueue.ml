open Core.Std
open Dict

type order = Equal | Less | Greater

module type COMPARABLE =
sig
  type t
  val compare : t -> t -> order
  val to_string : t -> string

  (* See the testing.ml for an explanation of
   * what these "generate*" functions do, and why we included them in
   * this signature. *)

  (* Generate a value of type t *)
  val generate: unit -> t

  (* Generate a value of type t that is greater than the argument. *)
  val generate_gt: t -> unit -> t

  (* Generate a value of type t that is less than the argument. *)
  val generate_lt: t -> unit -> t

  (* Generate a value of type t that is between argument 1 and argument 2.
   * Returns None if there is no value between argument 1 and argument 2. *)
  (* val generate_between: t -> t -> unit -> t option *)
end

(* An example implementation of the COMPARABLE signature. In this
 * example, the value of the integer also gives its priority. *)
module PtCompare : COMPARABLE with type t=string * float * BoolDict.dict =
struct
  type t = string * float * BoolDict.dict

  let compare x y = let (_, dist1, _), (_, dist2, _) = (x, y) in 
		    if dist1 < dist2 then Less 
		    else if dist1 > dist2 then Greater 
		    else Equal

  let to_string x = 
    let (n,d,dict) = x in 
    "(" ^ n ^ "," ^(Float.to_string d) ^ "," ^ (BoolDict.string_of_dict dict) ^ ")"

  let generate () = ("x", 0., BoolDict.empty)

  let generate_gt x () = 
    let (n, d,dict) = x in
    (n, d +. 1., dict)

  let generate_lt x () = 
    let (n, d, dict) = x in
    (n, d -. 1., dict)

  (* let generate_between x y () =
    let (n1, d1) = x in
    let (n2, d2) = y in
    let (lower, higher) = (min d1 d2, max d1 d2) in
    if higher - lower < 2 then None else Some (higher - 1) *)
end

(* A signature for a priority queue. The MINIMUM
 * valued element corresponds to the HIGHEST priority. For example,
 * in just an int prioqueue, the integer 4 has lower priority than
 * the integer 2.
 *)
module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* What's being stored in the priority queue *)
  type elt

  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : queue -> bool

  (* Takes an element and a queue, and returns a new queue with the
   * element added *)
  val add : elt -> queue -> queue

  (* Pulls the highest priority element out of the passed-in queue,
   * also returning the queue with that element
   * removed. Can raise the QueueEmpty exception. *)
  val take : queue -> elt * queue

  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

module BinaryHeap(C : COMPARABLE) : PRIOQUEUE with type elt = C.t =
struct

  exception QueueEmpty 

  type elt = C.t

  (* Be sure to read the pset spec for hints and clarifications.
   *
   * Remember the invariants of the tree that make up your queue:
   * 1) A tree is ODD if its left subtree has 1 more node than its right
   * subtree. It is EVEN if its left and right subtrees have the same number of
   * nodes. The tree can never be in any other state. This is the WEAK
   * invariant, and should never be false.
   *
   * 2) All nodes in the subtrees of a node should be *greater* than (or equal
   * to) the value of that node. This, combined with the previous invariant,
   * makes a STRONG invariant. Any tree that a user passes in to your module
   * and receives back from it should satisfy this invariant.  However, in the
   * process of, say, adding a node to the tree, the tree may intermittently
   * not satisfy the order invariant. If so, you *must* fix the tree before
   * returning it to the user.  Fill in the rest of the module below!
   *)
  (* A node in the tree is either even or odd *)
  type balance = Even | Odd

  (* A tree is either just a single element, has one branch (the first elt in
   * the tuple is the element at this node, and the second elt is the element
   * down the branch), or has two branches (with the node being even or odd) *)
  type tree =   TwoBranch of balance * elt * tree * tree
              | OneBranch of elt * elt
              | Leaf of elt

  (* A queue is either empty, or a tree *)
  type queue = Empty | Tree of tree

  let empty = Empty

  let is_empty (q : queue) = q = Empty

  (* calculates size of a tree)
  let rec size (t : tree) : int = 
    match t with
    | Leaf _ -> 1
    | OneBranch (_,_) -> 2
    | TwoBranch (_,_,t1,t2) -> 1 + (size t1) + (size t2)

  (* ensures tree satisfies the weak invariant *)
  let satisfies (t : tree) : bool = 
    match t with
    | TwoBranch (b, _, t1, t2) ->
       (match b with
	| Even -> size t1 = size t2
	| Odd -> size t1 = size t2 + 1)
    | _ -> true
   *)

  (* Adds element e to the queue q *)
  let add (e : elt) (q : queue) : queue =
    (* Given a tree, where e will be inserted is deterministic based on the
     * invariants. If we encounter a node in the tree where its value is greater
     * than the element being inserted, then we place the new elt in that spot
     * and propagate what used to be at that spot down toward where the new
     * element would have been inserted *)
    let rec add_to_tree (e : elt) (t : tree) : tree =
      match t with
      (* If the tree is just a Leaf, then we end up with a OneBranch *)
      | Leaf e1 ->
        (match C.compare e e1 with
         | Equal | Greater -> OneBranch (e1, e)
         | Less -> OneBranch (e, e1))

      (* If the tree was a OneBranch, it will now be a TwoBranch *)
      | OneBranch(e1, e2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
         | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

      (* If the tree was even, then it will become an odd tree (and the element
       * is inserted to the left *)
      | TwoBranch(Even, e1, t1, t2) ->
        (match C.compare e e1 with
         | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
         | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

      (* If the tree was odd, then it will become an even tree (and the element
       * is inserted to the right *)
      | TwoBranch(Odd, e1, t1, t2) ->
        match C.compare e e1 with
        | Equal | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
        | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
    in
    (* If the queue is empty, then e is the only Leaf in the tree.
     * Else, insert it into the proper location in the pre-existing tree *)
    match q with
    | Empty -> Tree (Leaf e)
    | Tree t -> Tree (add_to_tree e t)

  (* Simply returns the top element of the tree t (i.e., just a single pattern
   * match in *)
  let get_top (t : tree) : elt = 
    match t with
    | Leaf e -> e
    | OneBranch (e, _) -> e
    | TwoBranch (_, e, _, _) -> e

  (* Takes a tree, and if the top node is greater than its children, fixes
   * it. If fixing it results in a subtree where the node is greater than its
   * children, then you must (recursively) fix this tree too. *)
  let rec fix (t : tree) : tree = 
    match t with
    | Leaf e -> Leaf e
    (* swaps two elements if out of order *)
    | OneBranch (e1, e2) -> 
       (if C.compare e1 e2 = Greater then 
	  OneBranch (e2, e1)
	else t)
    | TwoBranch (b, e, t1, t2) -> 
       (let et1 = get_top t1 in
	let et2 = get_top t2 in
        (* check to see whether the element on top is out of place *)
	if C.compare e et1 = Greater || C.compare e et2 = Greater then
          (* pick highest priority element in the branches to swap with *) 
	  if C.compare et1 et2 = Less then 
	  (match t1 with 
	   | Leaf _ -> TwoBranch (b, et1, Leaf e, t2)
	   | OneBranch (_, e2) -> 
	      TwoBranch (b, et1, fix (OneBranch (e, e2)), t2)
	   | TwoBranch (b', _, t1', t2') -> 
	      TwoBranch (b, et1, fix (TwoBranch(b', e, t1', t2')), t2))
	  else match t2 with
	       | Leaf _ -> TwoBranch (b, et2, t1, Leaf e)
	       | OneBranch (_, e2) -> 
		  TwoBranch (b, et2, t1, fix (OneBranch (e, e2)))
	       | TwoBranch (b', _, t1', t2') -> 
      		  TwoBranch (b, et2, t1, fix (TwoBranch(b', e, t1', t2')))
	else t)
       
  let extract_tree (q : queue) : tree =
    match q with
    | Empty -> raise QueueEmpty
    | Tree t -> t

  (* Takes a tree, and returns the item that was most recently inserted into
   * that tree, as well as the queue that results from removing that element.
   * Notice that a queue is returned (since removing an element from just a leaf
   * would result in an empty case, which is captured by the queue type
   *
   * By "item most recently inserted", we don't mean the
   * most recently inserted *value*, but rather the newest node that was
   * added to the bottom-level of the tree. If you follow the implementation
   * of add carefully, you'll see that the newest value may end up somewhere
   * in the middle of the tree, but there is always *some* value brought
   * down into a new node at the bottom of the tree. *This* is the node
   * that we want you to return.
   *)

  let rec get_last (t : tree) : elt * queue = 
    match t with 
    | Leaf e -> (e, empty)
    | OneBranch (e1, e2) -> (e2, Tree (Leaf e1))
    | TwoBranch (Even, e, t1, t2) -> 
       (let (last, q) = get_last t2 in
	match q with
	| Empty -> (last, Tree (OneBranch (e, get_top t1)))
	| Tree t2' -> (last, Tree (TwoBranch(Odd, e, t1, t2'))))
    | TwoBranch (Odd, e, t1, t2) -> 
       (let (last, q) = get_last t1 in
	match q with
	| Empty -> (last, Tree (Leaf e))
	| Tree t1' -> (last, Tree (TwoBranch(Even, e, t1', t2))))
	 
  (* Implements the algorithm described in the writeup. You must finish this
   * implementation, as well as the implementations of get_last and fix, which
   * take uses *)
  let take (q : queue) : elt * queue =
    match extract_tree q with
    (* If the tree is just a Leaf, then return the value of that leaf, and the
     * new queue is now empty *)
    | Leaf e -> e, Empty

    (* If the tree is a OneBranch, then the new queue is just a Leaf *)
    | OneBranch (e1, e2) -> e1, Tree (Leaf e2)

    (* Removing an item from an even tree results in an odd tree. This
     * implementation replaces the root node with the most recently inserted
     * item, and then fixes the tree that results if it is violating the
     * strong invariant *)
    | TwoBranch (Even, e, t1, t2) ->
      let (last, q2') = get_last t2 in
      (match q2' with
       (* If one branch of the tree was just a leaf, we now have just
        * a OneBranch *)
       | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
       | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
    (* Implement the odd case! *)
    | TwoBranch (Odd, e, t1, t2) ->
       let (last, q2') = get_last t1 in
       (match q2' with 
	| Empty -> (e, Tree (fix (Leaf last)))
	| Tree t1' -> (e, Tree (fix (TwoBranch (Even, last, t1', t2)))))

  let test_take () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let after_ins = Tree (TwoBranch(Even, x3,
				      OneBranch (x2, x), 
				      OneBranch (x2, x2))) in
      (assert (take after_ins = (x3, Tree(TwoBranch(Odd, x2, 
						    OneBranch(x2, x),
						    Leaf x2)))))

  let test_get_last () =
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let after_ins = TwoBranch(Even, x3, 
				OneBranch (x3, x2),
				OneBranch (x2, x)) in
      assert (get_last after_ins = (x, Tree(TwoBranch(Odd, x3, 
						      OneBranch (x3, x2), 
						      Leaf x2))))

  let test_fix () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let after_ins = TwoBranch(Even, x, 
				OneBranch (x3, x2), 
				OneBranch (x3, x2)) in
      assert (fix after_ins = TwoBranch(Even, x3,
					OneBranch (x3, x2), 
					OneBranch (x2, x)))

  let test_get_top () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let after_ins = TwoBranch(Even, x3,
				OneBranch (x3, x2),
				OneBranch (x2, x)) in
      assert (get_top after_ins = x3)

  let run_tests () = 
    test_take();
    test_get_last();
    test_fix();
    test_get_top();
    ()
end
