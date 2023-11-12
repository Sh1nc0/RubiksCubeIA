open Batteries  


(**
Module     :  Various Functions Used in this Library
Description:  
@author Copyright  :  (c) February 28, 2019, José Martinez, École polytechnique de l'université de Nantes
License    :  AllRightsReserved
Maintainer :  Julien Cohen for the OCaml version
Stability  :  Experimental, Linux
Portability:  Ocaml 4.14, Ocaml 5.1

This module groups together a number of functions that are not present in standard libraries but used as utilities in various parts of the package.
*)

(**
 | A function that returns the pairs of consecutive elements in a list.

{b Example.}

 >>> consecutives "hello"

 returns

 > [('h','e'),('e','l'),('l','l'),('l','o')]

{b Post-condition.}

    * The tail of the list can be rebuilt from the second components of the pairs.

    * Unless the list contains a single element, it can be rebuilt from the pairs.

{b Time complexity.} O(n) where n is the number of elements in the list.
*)
(**consecutives
   :: [a]       ^ xs, a list of elements
   -> [(a, a)]  ^ the list of pairs of consecutive elements of xs*)
let rec consecutives xs = match xs with
  | x::y::r -> (x, y) :: consecutives (y::r)
  | _ -> []


(**
 | A function that mimics part of the Prolog "append" predicate.
 It splits a list at every index, returning a list of pairs of sub-lists.

{b Example.}

 >>> splits "hello"

 returns

 > [("","hello"),("h","ello"),("he","llo"),("hel","lo"),("hell","o"),("hello","")]

{b Post-condition.}

    * The concatenation of each pair gives back the original list.

{b Time complexity.} O(n) where n is the number of elements in the list.
 *)
(*
splits
   :: [a]           ^ xs, a list of elements
   -> [([a], [a])]  ^ (ys, zs), the decompositions of xs into two sub-lists, including the case where one of them is empty*)
let rec splits xs  = match xs with
   [] -> []
  | [x]-> [ ([], [x]) ; ([x], []) ]
  |  x::xs -> ([], x::xs) :: List.map (fun (ys, zs) -> (x::ys, zs)) (splits xs)

(**
 | An extension of 'splits' that splits a list into three consecutive sub-lists.

{b Example.}

 >>> splits3 "hello"

 returns a lengthy

 > [("","","hello"),("","h","ello"),("","he","llo"),("","hel","lo"),("","hell","o"),("","hello",""),("h","","ello"),("h","e","llo"),("h","el","lo"),("h","ell","o"),("h","ello",""),("he","","llo"),("he","l","lo"),("he","ll","o"),("he","llo",""),("hel","","lo"),("hel","l","o"),("hel","lo",""),("hell","","o"),("hell","o","")]

{b Post-condition.}

 The concatenation of the sub-lists gives back the original list.
*)
(*
splits3 :: [a]                ^ xs, a list of elements
        -> [([a], [a], [a])]  ^ (ys, zs, ts), the decompositions of xs into three consecutive, possibly empty, sub-lists*)


let splits3 xs = 
  let xs' = splits xs 
  in let xs'' = List.map 
                (fun (a,b) -> 
                  let tmp = splits b 
                  in List.map (fun (b,c) -> (a,b,c)) tmp)
                xs'
     in List.flatten xs'' ;;


(**
 | A predicate that determines whether all the elements of a list are different from each other.

{b Example.}

 >>> allDifferent "hello"

 returns

 > False

{b Time complexity.}  O(n^2) where n is the length of the list since we do not impose the 'Ord' constraint
*)

(*
allDifferent
   :: (Eq a)
   => [a]   ^ xs, a list of element, comparable by equality
   -> Bool  ^ true if, and only if, all the elements are pairwise distinct*)

let rec allDifferent eq xs = match xs with
    [] -> true
  | e::r -> not (List.exists (eq e) r) && allDifferent eq r 

(**
 | The Kronecker operator simply translates a boolean into 0 or 1.

{b Example.}

 >>> kronecker (allDifferent "hello")

 returns

 > 0
*)
(*kronecker
   :: Bool  ^ a boolean, False or True
   -> Int   ^ its translation as a natural integer, respectively 0 or 1*)
let kronecker b = match b with
    false -> 0
  | true  -> 1

let uncurry f = fun (x, y) -> f x y

let is_sorted ord xs = List.for_all (uncurry ord) (consecutives xs) 

(**
 | Checks whether a list of ordered elements appear in strictly increasing order.

{b Example.}

 >>> strictlyIncreasing "hello"

 returns

 > False
*)
(*
strictlyIncreasing
   :: (Ord a)
   => [a]
   -> Bool*)
let strictlyIncreasing xs = is_sorted (<) xs



let cartesianProduct xs ys = 
  let tmp = List.map (fun x -> List.map (fun y -> (x,y)) ys) xs
  in BatSet.of_list (List.flatten tmp)

(** Additional functions on lists *)

let rec last xs = match xs with
  | [] -> failwith "No last element in empty list"
  | e :: [] -> e
  | _ :: r -> last r
