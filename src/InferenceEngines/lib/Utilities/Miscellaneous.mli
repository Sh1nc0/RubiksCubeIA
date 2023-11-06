

(**
Various Functions Used in this Library

This module groups together a number of functions that are not present in standard libraries but used as utilities in various parts of the package.

@author :  (c) February 28, 2019, José Martinez, École polytechnique de l'université de Nantes

 *)

open Batteries

(**
A function that returns the pairs of consecutive elements in a list.

{b Example: }

[consecutives "hello"] returns
[ [('h','e'),('e','l'),('l','l'),('l','o')] ]


{b Post-condition: }

- The tail of the list can be rebuilt from the second components of the pairs.
- Unless the list contains a single element, it can be rebuilt from the pairs.

{b Time complexity} 

 O(n) where n is the number of elements in the list.


@param _ a list of elements. 

@return the list of pairs of consecutive elements of the parameter.

*)
val consecutives :
  'a list      
   -> ('a * 'a) list 



(** A function that mimics part of the Prolog  "append" predicate.
 It splits a list at every index, returning a list of pairs of sub-lists.

{b Example:}

[splits "hello"] returns [ [("","hello"),("h","ello"),("he","llo"),("hel","lo"),("hell","o"),("hello","")]]

{b Post-condition: }

- The concatenation of each pair gives back the original list.

{b Time complexity: }

O(n) where n is the number of elements in the list.


@param xs a list of elements 

@return the decompositions of xs into two sub-lists, including the case where one of them is empty.
 *)
val splits :
    'a list          
   -> ('a list * 'a list) list 


(** An extension of 'splits' that splits a list into three consecutive sub-lists.

{b Example: }
[splits "hello"]
 returns a lengthy
[[("","","hello");("","h","ello");("","he","llo");("","hel","lo");("","hell","o");("","hello","");("h","","ello");("h","e","llo");("h","el","lo");("h","ell","o");("h","ello","");("he","","llo");("he","l","lo");("he","ll","o");("he","llo","");("hel","","lo");("hel","l","o");("hel","lo","");("hell","","o");("hell","o","")]]

{b Post-condition: }
- The concatenation of the sub-lists gives back the original list.

@param xs a list of elements

@return he decompositions of xs into three consecutive, possibly empty, sub-lists.

Note that in the above example [("hello","","")] is not in the result but in practice we do not need it.
 *)
val splits3 : 'a list -> ('a list * 'a list * 'a list) list 


(** A predicate that determines whether all the elements of a list are different from each other.

{b Example: }
[allDifferent "hello"]

 returns

[False]

{b Time complexity:}

 O(n^2) where n is the length of the list since we do not impose the 'Ord' constraint


@param eq qn equality predicate
@param xs  a list of element, comparable by that equality

@return true if, and only if, all the elements are pairwise distinct.
*)
val allDifferent
   : ('a->'a->bool) 
   -> 'a list  
   -> bool 

(**
The Kronecker operator simply translates a boolean into 0 or 1.

{b Example:}

[kronecker (allDifferent "hello")]
returns
[0]

@param b a boolean, False or True

@return the translation of the argument as a natural integer, respectively 0 or 1.

*)
val kronecker
   : bool 
   -> int 

(** Checks whether a list of ordered elements appear in a given order.

{b Example:}

[is_sorted (<) [1;2;0]] 
returns
[False]


@param ord a comparison predicate
@param xs a list of elements 
*)
val is_sorted
   : ('a -> 'a -> bool) 
   -> 'a list
   -> bool

(** Checks whether a list of ordered elements appear in increasing order. Uses standard (<) as comparison for {{!is_sorted} is_sorted} above. 

{b Example:}

[strictlyIncreasing [1;2;0]] 
returns
[False]

@param xs a list of elements 

*)
val strictlyIncreasing
   : 'a list
   -> bool



(** {2 Cartesian product} *)



(**  Set of all the couples that can be built from two sets. We use pervasive polymorphic comparaison (<).

We use [BatSet.t] to represent sets, form the library "OCaml Batteries Included" instead of the module [Set] of the standard library because it is easier to use when we don't need specific comparison operations.

{b Nota} Since we are actually working with lists, there can be duplicates in them.

@param xs : a collection of some elements
@param ys : a collection of other elements

@return a set containing all the couples consisting of an element of [xs] and an element of [ys].
 
*)
val cartesianProduct : 'a list -> 'b list -> ('a*'b) BatSet.t

(** {2 Additional functions on lists} *)

val last : 'a list -> 'a
