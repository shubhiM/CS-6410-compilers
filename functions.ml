(* Returns nth term in the fibonnaci sequence *)
let rec fibonnaci (n :  int) :  int =
  if n <=1 then n
  else (fibonnaci (n - 1)) + (fibonnaci (n - 2));;

(* Subsitution based evaluation of (fibonnaci 3) *)
(* (fibonnaci 3)
=> (if 3<=1 then 3 else (fibonnaci (3 - 1)) + (fibonnaci (3 - 2)))
=> (fibonnaci (3-1)) + (fibonnaci (3 - 2))
=> (fibonnaci 2) + (fibonnaci (3 - 2))
=> (if 2<=1 then 2 else (fibonnaci (2 - 1)) + (fibonnaci (2 - 2))) + 
      (fibonnaci (3 - 2))
=> (fibonnaci (2 - 1)) + (fibonnaci (2 - 2)) + 
      (fibonnaci (3 - 2))
=> (fibonnaci 1) + (fibonnaci (2 - 2)) + 
      (fibonnaci (3 - 2))
=> (if 1 <= 1 then 1 else (fibonnaci (1 - 1)) + (fibonnaci (1 - 2))) +
      (fibonnaci (2 - 2)) + (fibonnaci (3 - 2))
=> 1 + (fibonnaci (2 - 2)) + (fibonnaci (3 - 2))
=> 1 + (fibonnaci 0) + (fibonnaci (3 - 2))
=> 1 + (if 0<=1 then 0 else (fibonnaci (0 - 1)) (fibonnaci (0 - 2)) + 
         (fibonnaci (3 - 2))
=> 1 + 0 + (fibonnaci (3 - 2))
=> 1 + 0 + (fibonnaci 1)
=> 1 + 0 + (if 1<=1 then 1 else (fibonnaci (1 - 1)) + (fibonnaci (1 - 2)))
=> 1 + 0 + 1
=> 2
 *)

(* Type definition of Binary tree *)
type btnode =
  | Leaf
  | Node of string *btnode *btnode

(* Returns inorder traversal string for the given binary tree *)
let rec inorder_str (bt : btnode) : string =
  match bt with
  | Leaf -> ""
  | Node(s, left, right) ->
     (inorder_str left) ^ s ^ (inorder_str right);;

(* Substitution based evaluation of 
   (inorder_str Node("a", Node("b", Leaf, Leaf), Node("c", Leaf, Leaf))) *)

(* (inorder_str Node("a", Node("b", Leaf, Leaf), Node("c", Leaf, Leaf)))
=> ( match Node("a", Node("b", Leaf, Leaf), Node("c", Leaf, Leaf)) with
       | Leaf -> ""
       | Node(s, left, right) =>
          (inorder_str left) ^ s ^ (inorder_str right))
=> (inorder_str left) ^ s ^ (inorder_str right)
=> (inorder_str Node("b", Leaf, Leaf)) ^ s ^ (inorder_str right)
=> ( match Node("b", Leaf, Leaf) with 
      | Leaf -> ""
      | Node(s, left, right) => 
          (inorder_str left) ^ s ^ (inorder_str right) ) 
   ^ s ^ (inorder_str right) 
=> ((inorder_str left) ^ s ^ (inorder_str right))   
   ^ s ^ (inorder_str right)
=> ((inorder_str Leaf) ^ s ^ (inorder_str Leaf))
   ^ s ^ (inorder_str right)
=> ...
=> ("" ^ "b" ^ "") ^ s ^ (inorder_str right)
=> "b" ^ s ^ (inorder_str right)
=> "b" ^ "a" ^ (inorder_str right)
=> "b" ^ "a" ^ (inorder_str Node("c", Leaf, Leaf))
=> "b" ^ "a" ^ ( match Node("c", Leaf, Leaf) with
                   | Leaf => ""
                   | Node(s, left, right) =>
                       (inorder_str left) ^ s ^ (inorder_str right))
=> "b" ^ "a" ^ ((inorder_str left) ^ s ^ (inorder_str right))
=> "b" ^ "a" ^ ((inorder_str Leaf) ^ s ^ (inorder_str right))
=> ...
=> "b" ^ "a" ^ ("" ^ s ^ (inorder_str right))
=> "b" ^ "a" ^ ("" ^ "c" ^ (inorder_str right))
=> "b" ^ "a" ^ ("" ^ "c" ^ (inorder_str Leaf))
=> ...   
=> "b" ^ "a" ^ ("" ^ "c" ^ "")
=> "b" ^ "a" ^ "c"
=> "bac"
*)
(* Returns number of nodes in the given binary tree *)
let rec size (bt : btnode) : int =
  match bt with
  | Leaf -> 0
  | Node(s, left, right) ->
     (1 + (size left) + (size right));;

(* Returns the height of the given binary tree *)
let rec height (bt : btnode) : int =
  match bt with
  | Leaf -> 0
  | Node(s, left, right) ->
     let height_left_subtree = (height left) in
     if height_left_subtree == 0 then (1 + (height right))
     else (1 + height_left_subtree);;

(* Returns a new list with each element incremented by one *)
let rec increment_all (l : int list) : int list =
  match l with
  | [] -> []
  | first::rest -> [(first + 1)] @ (increment_all rest);;

(* Returns a new string list that contains all that strings of
length greater than n *)
let rec long_strings (l : string list) (n : int) : string list =
  match l with
  | [] -> []
  | first::rest ->
     if String.length first > n then [first] @ (long_strings rest n)
     else (long_strings rest n);;

(* Returns a new list of given type that contains every other element
from the list starting with the first element *)
let rec every_other (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | first::rest ->
     [first] @ match rest with
               | [] -> []
               | first::rest -> (every_other rest);;

(* helper to sum all elements in the given list of integers *)
let rec sum (l : int list) : int =
  match l with
  | [] -> 0
  | first::rest ->
     first + (sum rest);;

(* Returns an int list that contains sums of the sub-lists *)
let rec sum_all (l : int list list) : int list =
  match l with
  | [] -> []
  | first::rest ->
     [(sum first)] @ (sum_all rest);;
