open Core.Std
open Order

(* Interfaces and implementations of dictionaries.  A dictionary
 * is used to associate a value with a key.  
 *)
module type DICT =
sig
  type key
  type value
  type dict

  (* An empty dictionary *)
  val empty : dict

  (* Reduce the dictionary using the provided function f and base case u.
   * Has the type: key -> value -> 'a -> 'a
   * and our base case u has type 'a.
   *
   * If our dictionary is the (key,value) pairs (in any order)
   *      (k1,v1), (k2,v2), (k3,v3), ... (kn,vn)
   * then fold should return:
   *      f k1 v1 (f k2 v2 (f k3 v3 (f ... (f kn vn u))))
   *)
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a

  (* Returns as an option the value associated with the provided key. If
   * the key is not in the dictionary, return None. *)
  val lookup : dict -> key -> value option

  (* Returns true if and only if the key is in the dictionary. *)
  val member : dict -> key -> bool

  (* Inserts a (key,value) pair into our dictionary. If the key is already
   * in our dictionary, update the key to have the new value. *)
  val insert : dict -> key -> value -> dict

  (* Removes the given key from the dictionary. If the key is not present,
   * return the original dictionary. *)
  val remove : dict -> key -> dict

  (* Return an arbitrary key, value pair along with a new dict with that
   * pair removed. Return None if the input dict is empty *)
  val choose : dict -> (key * value * dict) option

  (* Functions to convert our types to strings for debugging and logging *)
  val string_of_key: key -> string
  val string_of_value : value -> string
  val string_of_dict : dict -> string

  (* Runs all the tests. *)
  val run_tests : unit -> unit
end



(* Argument module signature to our DICT functors *)
module type DICT_ARG =
sig
  type key
  type value
  val compare : key -> key -> Ordering.t
  val string_of_key : key -> string
  val string_of_value : value -> string
  
  (* Testing functions *)

  (* Generate a key. The same key is always returned *)
  val gen_key : unit -> key

  (* Generate a random key. *)
  val gen_key_random : unit -> key

  (* Generates a key greater than the argument. *)
  val gen_key_gt : key -> unit -> key

  (* Generates a random value. *)
  val gen_value : unit -> value

  (* Generates a random (key,value) pair *)
  val gen_pair : unit -> key * value
end


(* An example implementation of our DICT_ARG signature. We use this struct
 * for testing. *)
module IntStringDictArg : DICT_ARG =
struct
  open Order
  type key = int
  type value = string
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_key = string_of_int
  let string_of_value v = v
  let gen_key () = 0
  let gen_key_gt x () = x + 1
  let gen_key_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)

  (* returns the nth string in lst, or "cow" n > length of list *)
  let rec lst_n (lst: string list) (n: int) : string =
    match lst with
      | [] -> "cow"
      | hd::tl -> if n = 0 then hd else lst_n tl (n-1)

  (* list of possible values to generate *)
  let possible_values = ["a";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";
                         "o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z";
                         "zzzzzz";"cheese";"foo";"bar";"baz";"quux";"42"]
  let num_values = List.length possible_values
  (* gen_value will return the string at this current index *)
  let current_index = ref 0
  let gen_value () =
    let index = !current_index in
    if index >= num_values then
      (current_index := 0; lst_n possible_values index)
    else
      (current_index := index + 1; lst_n possible_values index)
  let gen_pair () = (gen_key_random(), gen_value())
end


(******************************************************************)
(* BTDict: a functor that implements our DICT signature           *)
(* using a balanced tree (2-3 trees)                              *)
(******************************************************************)
module BTDict(D:DICT_ARG) : (DICT with type key = D.key
with type value = D.value) =
struct
  open Order

  type key = D.key
  type value = D.value

  (* A dictionary entry is a (key,value) pair. We compare two (key,value)
   * pairs with the provided key-comparison function D.compare. For example,
   * we may choose to keep a dictionary mapping links to their ranks. In this
   * case, our (key,value) pairs will be (link,rank) pairs, and we compare
   * links using string comparison. *)
  type pair = key * value

  (* Type definition for dictionary, which we choose to represent as a 2-3 Tree.
   * A Three-node contains two pairs and three subtrees: left, middle, and
   * right, represented by the 3 dicts in the definition below. *)
  type dict =
    | Leaf
    | Two of dict * pair * dict
    | Three of dict * pair * dict * pair * dict

  (* INVARIANTS:
   * 2-node: Two(left,(k1,v1),right)
   * (1) Every key k appearing in subtree left must be k < k1.
   * (2) Every key k appearing in subtree right must be k > k1.
   * (3) The length of the path from the 2-node to
   *     every leaf in its two subtrees must be the same.
   *
   * 3-node: Three(left,(k1,v1),middle,(k2,v2),right)
   * (1) k1 < k2.
   * (2) Every key k appearing in subtree left must be k < k1.
   * (3) Every key k appearing in subtree right must be k > k2.
   * (4) Every key k appearing in subtree middle must be k1 < k < k2.
   * (5) The length of the path from the 3-node to every leaf in its three
   *     subtrees must be the same.
   *)

  (* FOR INSERTION:
   * A kicked configuration returned by going downwards on insertion.
   * We can only kick up Two nodes, hence Up takes a dict * pair * dict *)
  type kicked =
    | Up of dict * pair * dict
    | Done of dict

  (* FOR REMOVAL:
   * A hole configuration returned by going downwards on removal. We
   * include a pair option whenever we remove the minimum of the right
   * subtree of the current pair in order the current pair *)
  type hole =
    | Hole of pair option * dict
    | Absorbed of pair option * dict

  (* FOR REMOVAL:
   * A direction will distinguish which configuration we came from in the
   * removal cases. We use direction2 for cases (1-2) on the handout, and
   * we use direction3 for cases (3-4) on the handout. *)
  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3

  let empty : dict = Leaf

  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    match d with
      | Leaf -> u
      | Two(left,(k1,v1),right) ->
        f k1 v1 (fold f (fold f u left) right)
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        f k1 v1
          (f k2 v2
             (fold f
                (fold f
                   (fold f u left) middle) right))

  let string_of_key = D.string_of_key
  let string_of_value = D.string_of_value
  let string_of_dict (d: dict) : string =
    fold (fun k v rest -> (string_of_key k)^ (string_of_value v) ^ rest) "" d

  (* Upward phase for w where its parent is a Two node whose (key,value) is x.
   * One of x's children is w, and the other child is x_other. This function
   * should return a kicked-up configuration containing the new tree as a
   * result of performing the upward phase on w. *)
   let insert_upward_two (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (x_other: dict) : kicked =
    let (w_key,_) = w in
    let (x_key,_) = x in
    match D.compare w_key x_key with
      | Equal -> failwith "Duplicate key not handled in downward phase"
      | Less -> Done(Three(w_left,w,w_right,x,x_other))
      | Greater -> Done(Three(x_other,x,w_left,w,w_right))

  (* Upward phase for w where its parent is a Three node whose (key,value) is x. *)
  let insert_upward_three (w: pair) (w_left: dict) (w_right: dict)
      (x: pair) (y: pair) (other_left: dict) (other_right: dict) : kicked =
    let (w_key,_) = w in
    let (x_key,_) = x in
    let (y_key,_) = y in
    match D.compare w_key x_key, D.compare w_key y_key with
      | Equal, _ -> failwith "Duplicate key not handled in downward phase"
      | _, Equal -> failwith "Duplicate key not handled in downward phase"
      | Less, _ ->
        let left = Two(w_left,w,w_right) in
        let right = Two(other_left,y,other_right) in
        Up(left,x,right)
      | _, Greater ->
        let left = Two(other_left,x,other_right) in
        let right = Two(w_left,w,w_right) in
        Up(left,y,right)
      | Greater, Less ->
        let left = Two(other_left,x,w_left) in
        let right = Two(w_right,y,other_right) in
        Up(left,w,right)

  (* Downward phase for inserting (k,v) into our dictionary d.*)

  (* insert_downward should handle the base case when inserting into a Leaf,
   * and if our dictionary d is a Two-node or a Three-node, we call the
   * corresponding functions insert_downward_two or insert_downward_three
   * with the appropriate arguments. *)
  let rec insert_downward (d: dict) (k: key) (v: value) : kicked =
    match d with
      | Leaf -> Up(Leaf,(k,v),Leaf)
      | Two(left,n,right) ->
        insert_downward_two (k,v) n left right
      | Three(left,n1,middle,n2,right) ->
        insert_downward_three (k,v) n1 n2 left middle right

  (* Downward phase on a Two node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) is the (key,value) of the current Two node, and left and right
   * are the two subtrees of the current Two node. *)
  and insert_downward_two ((k,v): pair) ((k1,v1): pair)
      (left: dict) (right: dict) : kicked =
    match D.compare k k1 with
      | Equal -> Done(Two(left,(k1,v),right))
      | Less ->
        (match insert_downward left k v with
          | Up(l_kick,w,r_kick) ->
            insert_upward_two w l_kick r_kick (k1,v1) right
          | Done new_left -> Done(Two(new_left,(k1,v1),right))
        )
      | Greater ->
        (match insert_downward right k v with
          | Up(l_kick,w,r_kick) ->
            insert_upward_two w l_kick r_kick (k1,v1) left
          | Done new_right -> Done(Two(left,(k1,v1),new_right))
        )

  (* Downward phase on a Three node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) and (k2,v2) are the two (key,value) pairs in our Three node, and
   * left, middle, and right are the three subtrees of our current Three node *)
  and insert_downward_three ((k,v): pair) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : kicked =
    match D.compare k k1, D.compare k k2 with
      | Equal, _ -> Done(Three(left,(k1,v),middle,(k2,v2),right))
      | _, Equal -> Done(Three(left,(k1,v1),middle,(k2,v),right))
      | Less, _ ->
        (match insert_downward left k v with
          | Up(l_kick,w,r_kick) ->
            insert_upward_three w l_kick r_kick (k1,v1) (k2,v2) middle right
          | Done new_left -> Done(Three(new_left,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match insert_downward right k v with
          | Up(l_kick,w,r_kick) ->
            insert_upward_three w l_kick r_kick (k1,v1) (k2,v2) left middle
          | Done new_right -> Done(Three(left,(k1,v1),middle,(k2,v2),new_right))
        )
      | Greater, Less ->
        (match insert_downward middle k v with
          | Up(l_kick,w,r_kick) ->
            insert_upward_three w l_kick r_kick (k1,v1) (k2,v2) left right
          | Done new_mid -> Done(Three(left,(k1,v1),new_mid,(k2,v2),right))
        )

  (* We insert (k,v) into our dict using insert_downward, which gives us
   * "kicked" up configuration. We return the tree contained in the "kicked"
   * configuration. *)
  let insert (d: dict) (k: key) (v: value) : dict =
    match insert_downward d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x

  (* Upward phase for removal where the parent of the hole is a Two node.
   * See cases (1-2) on the handout. n is the (key,value) pair contained in
   * the parent node; left and right are the subtrees of the parent node (our
   * hole is one of these subtrees); and dir indicates which subtree was
   * contained by the hole. *)
  let remove_upward_two (n: pair) (rem: pair option)
      (left: dict) (right: dict) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) -> Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d -> Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  (* Upward phase for removal where the parent of the hole is a Three node.
   * See cases (3-4) on the handout. n1 and n2 are the (key,value) pairs
   * contained in the parent node; left, middle, and right are the subtrees
   * of the parent node (our hole is one of these subtrees); and dir indicates
   * which subtree was the tree contained by the hole. *)
  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: dict) (middle: dict) (right: dict) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e ->
        Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e ->
        Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) ->
        Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e ->
        Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  let rec remove_downward (d: dict) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        (match D.compare k k1 with
          | Equal -> Hole(Some(k1,v1),Leaf)
          | Less | Greater -> Absorbed(None,d)
        )
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (match D.compare k k1, D.compare k k2 with
          | Equal, _ -> Absorbed(Some(k1,v1),Two(Leaf,(k2,v2),Leaf))
          | _, Equal -> Absorbed(Some(k2,v2),Two(Leaf,(k1,v1),Leaf))
          | _, _ -> Absorbed(None,d)
        )
      | Two(l,n,r) -> remove_downward_two k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three k n1 n2 l m r

  and remove_downward_two (k: key) ((k1,v1): pair)
      (left: dict) (right: dict) : hole =
    match D.compare k k1 with
      | Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,left)
          | Hole(Some n,new_right) ->
            remove_upward_two n None left new_right Right2
          | Absorbed(None,_) -> Hole(None,left)
          | Absorbed(Some n,new_right) -> Absorbed(None,Two(left,n,new_right))
        )
      | Less ->
        (match remove_downward left k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,(k1,v1),right))
        )
      | Greater ->
        (match remove_downward right k with
          | Hole(rem,t) -> remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) -> Absorbed(rem,Two(left,(k1,v1),t))
        )

  and remove_downward_three (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: dict) (middle: dict) (right: dict) : hole =
    match D.compare k k1, D.compare k k2 with
      | Equal, _ ->
        (match remove_min middle with
          | Hole(None,_) -> Hole(None,Two(left,(k2,v2),right))
          | Hole(Some n,new_middle) ->
            remove_upward_three n (k2,v2) None left new_middle right Mid3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),right))
          | Absorbed(Some n,new_middle) ->
            Absorbed(None,Three(left,n,new_middle,(k2,v2),right))
        )
      | _ , Equal ->
        (match remove_min right with
          | Hole(None,_) -> Hole(None,Two(left,(k1,v1),middle))
          | Hole(Some n,new_right) ->
            remove_upward_three (k1,v1) n None left middle new_right Right3
          | Absorbed(None,_) -> Absorbed(None,Two(left,(k1,v1),middle))
          | Absorbed(Some n,new_right) ->
            Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
        )
      | Less, _ ->
        (match remove_downward left k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
        )
      | _, Greater ->
        (match remove_downward right k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
        )
      | Greater, Less ->
        (match remove_downward middle k with
          | Hole(rem,t) ->
            remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
          | Absorbed(rem,t) ->
            Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
        )

  and remove_min (d: dict) : hole =
    match d with
      | Leaf -> Hole(None,Leaf)
      | Two(Leaf,n,_) -> Hole(Some n,Leaf)
      | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
      | Two(left,n,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_two n rem t right Left2
          | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
        )
      | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  let remove (d: dict) (k: key) : dict =
    match remove_downward d k with
      | Hole(_,d') -> d'
      | Absorbed(_,d') -> d'

   (* Returns the value of the given keyin our dictionary 
   * and returns it as an option, or return None
   * if the key is not in our dictionary. *)
  let rec lookup (d: dict) (k: key) : value option =
    match d with
      | Leaf -> None
      | Two(left,(k1,v1),right) ->
        (match D.compare k k1 with
          | Equal -> Some v1
          | Less -> lookup left k
          | Greater -> lookup right k)
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        (match D.compare k k1, D.compare k k2 with
          | Equal, _ -> Some v1
          | _, Equal -> Some v2
          | Less, _ -> lookup left k
          | _, Greater -> lookup right k
          | Greater, Less -> lookup middle k)

  (* Test if a given key is in our dictionary *)
  let member (d: dict) (k: key) : bool =
    lookup d k <> None

   (* Removes any (key,value) pair from our dictionary and returns
   * as an option this (key,value) pair along with the new dictionary.
   * If our dictionary is empty, this should return None. *)
  let choose (d: dict) : (key * value * dict) option =
    match d with
      | Leaf -> None
      | Two(_,(k,v),_) -> Some (k,v,remove d k)
      | Three(_,(k,v),_,_,_) -> Some (k,v,remove d k)

   (* Returns true if and only if the tree is "balanced"*)
 let balanced (d: dict) : bool =
    let rec bounds (d: dict) : (int * int) =
      match d with
        | Leaf -> (0,0)
        | Two(left,_,right) ->
          let (l_min,l_max) = bounds left in
          let (r_min,r_max) = bounds right in
          (1 + min l_min r_min, 1 + max l_max r_max)
        | Three(left,_,middle,_,right) ->
          let (l_min,l_max) = bounds left in
          let (m_min,m_max) = bounds middle in
          let (r_min,r_max) = bounds right in
          (1 + min (min l_min m_min) r_min, 1 + max (max l_max m_max) r_max)
    in
    let (min_height,max_height) = bounds d in
    max_height - min_height = 0
 

  (********************************************************************)
  (*       TESTS                                                      *)
  (********************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: dict) (lst: (key * value) list) : dict =
    List.fold_left lst ~f:(fun r (k,v) -> insert r k v) ~init:d

  (* adds a list of (key,value) pairs in right-to-left order *)
  let insert_list_reversed (d: dict) (lst: (key * value) list) : dict =
    List.fold_right lst ~f:(fun (k,v) r -> insert r k v) ~init:d

  (* generates a (key,value) list with n distinct keys in increasing order *)
  let generate_pair_list (size: int) : (key * value) list =
    let rec helper (size: int) (current: key) : (key * value) list =
      if size <= 0 then []
      else
        let new_current = D.gen_key_gt current () in
        (new_current, D.gen_value()) :: (helper (size - 1) new_current)
    in
    helper size (D.gen_key ())

  (* generates a (key,value) list with keys in random order *)
  let rec generate_random_list (size: int) : (key * value) list =
    if size <= 0 then []
    else
      (D.gen_key_random(), D.gen_value()) :: (generate_random_list (size - 1))

  let test_balance () =
    let d1 = Leaf in
    assert(balanced d1) ;

    let d2 = Two(Leaf,D.gen_pair(),Leaf) in
    assert(balanced d2) ;

    let d3 = Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf) in
    assert(balanced d3) ;

    let d4 = Three(Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),
                       D.gen_pair(),Two(Two(Leaf,D.gen_pair(),Leaf),
                                        D.gen_pair(),
                                        Two(Leaf,D.gen_pair(),Leaf))),
                   D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf))),D.gen_pair(),
                   Two(Two(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                           Two(Leaf,D.gen_pair(),Leaf)),D.gen_pair(),
                       Three(Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),
                             Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf))))
    in
    assert(balanced d4) ;

    let d5 = Two(Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf)) in
    assert(not (balanced d5)) ;

    let d6 = Three(Leaf,D.gen_pair(),
                   Two(Leaf,D.gen_pair(),Leaf),D.gen_pair(),Leaf) in
    assert(not (balanced d6)) ;

    let d7 = Three(Three(Leaf,D.gen_pair(),Leaf,D.gen_pair(),Leaf),
                   D.gen_pair(),Leaf,D.gen_pair(),Two(Leaf,D.gen_pair(),Leaf))
    in
    assert(not (balanced d7)) ;
    ()

  let test_insert_in_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter pairs1 ~f:(fun (k,v) -> assert(lookup d1 k = Some v)) ;
    assert(balanced d1) ;

    let pairs2 = generate_pair_list 13 in
    let d2 = insert_list empty pairs2 in
    List.iter pairs2 ~f:(fun (k,v) -> assert(lookup d2 k = Some v)) ;
    assert(balanced d2) ;
    ()

  let test_insert_empty () =
    let pairs3 = [] in
    let d3 = insert_list empty pairs3 in
    List.iter pairs3 ~f:(fun (k,v) -> assert(lookup d3 k = Some v)) ;
    assert(balanced d3) ;
    ()

  let test_insert_reversed_order () =
    let pairs6 = generate_pair_list 26 in
    let d6 = insert_list_reversed empty pairs6 in
    List.iter pairs6 ~f:(fun (k,v) -> assert(lookup d6 k = Some v)) ;
    assert(balanced d6) ;

    let pairs7 = generate_pair_list 13 in
    let d7 = insert_list_reversed empty pairs7 in
    List.iter pairs7 ~f:(fun (k,v) -> assert(lookup d7 k = Some v)) ;
    assert(balanced d7) ;
    ()

  let test_insert_update () =
    let pairs4 = generate_pair_list 26 in
    let update_key = D.gen_key_gt (D.gen_key_gt (D.gen_key ()) ()) () in
    let update_value = D.gen_value () in
    let new_pairs = pairs4 @ [(update_key,update_value)] in
    let d4 = insert_list empty new_pairs in
    List.iter
      new_pairs
      ~f:(fun (k,v) ->
        if k = update_key then assert(lookup d4 k = Some update_value)
        else assert(lookup d4 k = Some v)
      ) ;
    assert(balanced d4) ;
    ()

  let test_insert_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    List.iter pairs5 ~f:(fun (k,_) -> assert(member d5 k)) ;
    assert(balanced d5) ;
    ()

  let test_remove_from_nothing () =
    let d1 = empty in
    let r1 = remove d1 (D.gen_key()) in
    assert(r1 = empty) ;
    assert(balanced r1) ;
    ()

  let test_remove_in_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list empty pairs1 in
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        let _ = List.iter
          pairs1
          ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) in
        assert(balanced r)
      ) ;
    ()

  let test_remove_reverse_order () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    List.iter
      pairs1
      ~f:(fun (k,_) ->
        let r = remove d1 k in
        let _ = List.iter
          pairs1
          ~f:(fun (k2,v2) ->
            if k = k2 then assert(lookup r k2 = None)
            else assert(lookup r k2 = Some v2)
          ) in
        assert(balanced r)
      ) ;
    ()

  let test_remove_random_order () =
    let pairs5 = generate_random_list 100 in
    let d5 = insert_list empty pairs5 in
    let r5 = List.fold_right pairs5 ~f:(fun (k,_) d -> remove d k) ~init:d5 in
    List.iter pairs5 ~f:(fun (k,_) -> assert(not (member r5 k))) ;
    assert(r5 = empty) ;
    assert(balanced r5) ;
    ()

  let test_choose () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in

    let rec choose_n (n: int) (d: dict) : bool =
      if n = 0 then true
      else
        match choose d with
          | None -> false
          | Some (_,_,rest) -> choose_n (n-1) rest
    in
    assert(choose_n 26 d1)

  let test_fold () =
    let pairs1 = generate_pair_list 26 in
    let d1 = insert_list_reversed empty pairs1 in
    let num_elements = fold (fun _ _ r -> r + 1) 0 d1 in
    assert(num_elements = 26)
 
  let run_tests () = 
    test_balance() ;
    test_insert_in_order() ;
    test_insert_empty() ;
    test_insert_reversed_order () ;
    test_insert_update() ;
    test_insert_random_order() ;
    test_remove_from_nothing() ;
    test_remove_in_order() ;
    test_remove_reverse_order() ;
    test_remove_random_order() ;
    test_choose () ;
    test_fold () ; 
    ()

end

(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a dictionary mapping ints to strings using our
 * BTDict functor and run the tests *)

module IntStringBTDict = BTDict(IntStringDictArg) ;;
IntStringBTDict.run_tests();;

(******************************************************************)
(* Make: a functor that creates a DICT *)
(******************************************************************)
module Make (D:DICT_ARG) : (DICT with type key = D.key
  with type value = D.value) =
  BTDict(D)

(* Make all the modules used in Dijkstra's *)

(* Dictionary that maps a node to a boolean. Used to store the "visited" state-space *)
module BoolDict = Make(
  struct
    type key = string
    type value = bool
    let compare = string_compare
    let string_of_key x = x
    let string_of_value = Bool.to_string
    let gen_key () = "1"
    let gen_key_random () = Int.to_string (Random.int 100)
    let gen_key_gt x () = x ^ "a"
    let gen_value () = Random.bool ()
    let gen_pair () = (gen_key_random (), gen_value ())
  end)

(* Dictionary that maps a node to the shortest distance to that node from start node *)
module DistDict = Make(
  struct
    type key = string * BoolDict.dict
    type value = float
    let compare (x,dx) (y,dy) =
      let i = string_compare x y in 
      if i = Equal then 
        let dx_string = BoolDict.string_of_dict dx in
        let dy_string = BoolDict.string_of_dict dy in
<<<<<<< HEAD
        if String.length(dx_string) = String.length(dy_string) then
            if (BoolDict.fold (fun key _ y -> ((BoolDict.lookup dx key) = (BoolDict.lookup dy key)) && y) true dx) then
              Equal
=======
            if String.length(dx_string) = String.length(dy_string) then 
            let is_equal_1 = BoolDict.fold (fun key _ y -> ((BoolDict.lookup dy key) <> None) && y) true dx in
            let is_equal_2 = BoolDict.fold (fun key _ y -> ((BoolDict.lookup dx key) <> None) && y) true dy in
            if is_equal_1 && is_equal_2 then Equal 
>>>>>>> 9a4af16e007d72760dfa960f70f7cff68fa40996
            else string_compare dx_string dy_string
         else string_compare dx_string dy_string 
       else i         
    let string_of_key (x,dict) = x ^ BoolDict.string_of_dict dict
    let string_of_value = Float.to_string
    let gen_key () = ("1", BoolDict.empty)
    let gen_key_random () = (Int.to_string (Random.int 100), BoolDict.empty)
    let gen_key_gt (s, dict) () = (s ^ "a", dict)
    let gen_value () = Random.float 100.
    let gen_pair () = (gen_key_random (), gen_value ())
  end)

(* Dictionary that maps a node to the previous node in that specific path *)
module PrevDict = Make(
  struct
    type key = (string * BoolDict.dict)
    type value = (string * BoolDict.dict)
    let compare (x,dx) (y,dy) =
      let i = string_compare x y in 
      if i = Equal then 
        let dx_string = BoolDict.string_of_dict dx in
        let dy_string = BoolDict.string_of_dict dy in
        if String.length(dx_string) = String.length(dy_string) then 
            let is_equal_1 = BoolDict.fold (fun key _ y -> ((BoolDict.lookup dy key) <> None) && y) true dx in
            let is_equal_2 = BoolDict.fold (fun key _ y -> ((BoolDict.lookup dx key) <> None) && y) true dy in
         if is_equal_1 && is_equal_2 then Equal 
         else string_compare dx_string dy_string
        else string_compare dx_string dy_string 
      else i         
    let string_of_key (x,dict) = x ^ BoolDict.string_of_dict dict
    let string_of_value (x,dict) = x ^ BoolDict.string_of_dict dict
    let gen_key () = ("1", BoolDict.empty)
    let gen_key_random () = (Int.to_string (Random.int 100), BoolDict.empty)
    let gen_key_gt (s, dict) () = (s ^ "a", dict)
    let gen_value () = gen_key_random ()
    let gen_pair () = (gen_key_random (), gen_value ())    
  end)

(* Dictionary that maps a location's name to its coordinates *)
module LocationDict = Make(
  struct
    type key = string
    type value = string
    let compare = string_compare
    let string_of_key x = x
    let string_of_value x = x
    let gen_key () = "1"
    let gen_key_random () = Int.to_string (Random.int 100)
    let gen_key_gt x () = x ^ "a"
    let gen_value () = Int.to_string (Random.int 100)
    let gen_pair () = (gen_key_random (), gen_value ())  
  end)


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
