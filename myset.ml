open Core.Std

(* Definitions for sets. *)

(* An interface for set modules *)

module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Ordering.t
  val string_of_t : t -> string
  
  (* testing functions *)
  
  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Equal
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
end

module StringComparable : COMPARABLE with type t = string = 
struct 
  open Order
  type t = string
  let compare x y = let i = String.compare x y in
    if i = 0 then Equal else if i > 0 then Greater else Less
  let string_of_t t = t
  let gen () = "1"
  let gen_random ()= Int.to_string (Random.int 100)
  let gen_gt x () = x ^ "a"
end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(
     struct
       type key = C.t
       type value = C.t
       let compare = C.compare
       let string_of_key = C.string_of_t
       let string_of_value = C.string_of_t
       let gen_key () = C.gen ()
       let gen_key_gt _ () = gen_key ()
       let gen_key_random () = gen_key ()
       let gen_value () = gen_key ()
       let gen_pair () = (gen_key(),gen_value())
     end)

  type elt = D.key
  type set = D.dict
  let empty = D.empty
  let is_empty xs = xs = D.empty
  let insert x d = D.insert d x x
  let singleton x = D.insert D.empty x x
  let fold f = D.fold (fun k _ z -> f k z)
  let union d1 d2 = fold insert d1 d2               
  let intersect d1 d2 = fold (fun k z -> 
           if D.member d2 k then 
             insert k z
           else z)
          D.empty
          d1
  let remove x d = D.remove d x
  let member d x = D.member d x
  let choose d = (
    match D.choose d with
    | None -> None 
    | Some (k, _, d) -> Some (k, d))
  let fold f = D.fold (fun k _ z -> f k z)
  let string_of_elt = D.string_of_key
  let string_of_set s = 
    let f = (fun k z -> z ^ "; " ^ (string_of_elt k)) in
    "set([" ^ (fold f "" s) ^ "])"

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  let insert_set (d: set) (lst: elt list) : set =
    List.fold_left lst ~f:(fun r k -> insert k r) ~init:d

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_set empty elts in
    List.iter elts ~f:(fun k -> assert(member s1 k)) ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_set empty elts in
    let s2 = List.fold_right elts ~f:(fun k r -> remove k r) ~init:s1 in
    List.iter elts ~f:(fun k -> assert(not (member s2 k))) ;
    ()

  let test_union () =
    let elts = generate_random_list 100 in
    let elts2 = generate_random_list 100 in
    let s1 = insert_set empty elts in
    let s2 = insert_set empty elts2 in
    let s3 = union s1 s2 in
    assert (union empty empty = empty);
    assert (union s3 empty = s3);
    List.iter elts ~f:(fun k -> assert(member s3 k));
    List.iter elts2 ~f:(fun k -> assert(member s3 k));
    ()

  let test_intersect () =
    let elts = generate_random_list 100 in
    let elts2 = generate_random_list 100 in
    let s1 = insert_set empty elts in
    let s2 = insert_set empty elts2 in
    let s3 = intersect s1 s2 in
    assert (intersect s1 empty = empty);
    List.iter elts2 ~f:(fun k -> if member s1 k then assert(member s3 k) 
         else assert (not (member s3 k)));
    ()

  let test_member () =
    let elts = generate_random_list 100 in
    let elts2 = generate_random_list 100 in
    let s1 = insert_set empty elts in
    let s2 = insert_set empty elts2 in
    let s3 = union s1 s2 in
    List.iter elts  ~f:(fun k -> assert(member s3 k));
    List.iter elts2  ~f:(fun k -> assert(member s3 k));
    ()

  let test_choose () =
    let elts = generate_random_list 100 in
    let s1 = insert_set empty elts in
    List.iter
      elts
      ~f:(fun _ ->
          let r = choose s1 in
    match r with
    | None -> assert (false)
    | Some (x,rest) -> assert ((not (member rest x)) && member s1 x)
   );
    ()

  let test_fold () =
    let elts = generate_random_list 100 in
    let elts2 = generate_random_list 100 in
    let s1 = insert_set empty elts in
    let s2 = insert_set empty elts2 in
    let s3 = union s1 s2 in
    assert (fold (fun x y -> member s3 x && y) true s1);
    assert (fold (fun x y -> member s3 x && y) true s2);
    assert (not (fold (fun x y -> member empty x && y) true s1));
    ()

  let test_is_empty () =
    let elts = generate_random_list 100 in
    let s1 = insert_set empty elts in
    assert (is_empty s1 = false);
    assert (is_empty empty);
    ()

  let test_singleton () =
    let x = C.gen_random() in
    let s1 = singleton x in
    assert (s1 = insert x empty);
    ()
 
  let run_tests () =
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()
end

(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;

(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  DictSet (C)

module DestinationSet = Make(StringComparable)
