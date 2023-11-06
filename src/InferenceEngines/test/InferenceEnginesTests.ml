
let _ = print_endline "Début des tests."


(* Test du module Misceanellous *)

open Miscellaneous

let res = cartesianProduct [1;2] [3;4] ;;

assert (BatSet.mem (1,3) res) ;;
assert (BatSet.mem (1,4) res) ;;

(* Préparation pour le test du module StateProblem *)

module IntState = 
  struct 
    type state = int
    let string_of_state = string_of_int
  end

module IntTransition =
  struct
    type op = int
    let string_of_op = string_of_int
  end


(* Test des modules StateProblem et Tree.Naive *)

module TrucProblem = 
  StateProblem.StateProblem (IntState) (IntTransition) (Additive.IntC)

module TrucNaiveSolver  = 
  Naive.Solver (TrucProblem) ;;



let p1 = TrucProblem.mkExtensionStateProblem
          ~ss:[1;2;3] ~i:1 ~fs:[3] ~es:[(1,0,2,100)] ;;

let s1 = TrucNaiveSolver.solver' p1 ;;

assert (s1=[]);;

let p2 = TrucProblem.mkExtensionStateProblem
          ~ss:[1;2;3] ~i:1 ~fs:[3] ~es:[(1,0,2,100);(2,0,3,100)]   ;;

let s2 = TrucNaiveSolver.solver' p2 ;;

assert (s2<>[]);;
assert (match s2 with
        | [] -> false
        | s::[] ->
           TrucProblem.solutionCost s = 200
        | _::_::_ -> false);;

(* Batteries *)

let _ = BatSet.empty


(* Klotski *)

let _ = Klotski.klotski_flag 

(* Klotski solved by Naive *)

let _ = print_endline "Début Klotski Naive"

let oracle r = 
  if r = [] 
  then print_endline "Pas de solution trouvée." 
  else List. iter (fun s -> print_endline (Klotski.KlotskiProblem.short_solution s)) r 

(*let oracle r = print_endline (string_of_int (List.length r) ^ " solution(s) trouvée(s).")
 *) 

module N = Naive.Solver (Klotski.KlotskiProblem)
         
let [@warning "-32"] naive_klotski p = N.solver Klotski.moves Klotski.isSolved p
  

(* Stack Overflow *)
(* À cause des cycles. Klotski n'est pas adapté à la recherche naive. *)
(*let res = naive_klotski Klotski.anotherKlotski*)

let _ = print_newline ()

(* Klotski Solved by DepthLimit *)

let prof = 4
let _ = print_endline ("Début Klotski Limit (" ^ string_of_int prof ^ ")")

module DL = DepthLimit.Solver (Klotski.KlotskiProblem)
let limit_klotski n p = DL.solver n Klotski.moves Klotski.isSolved p

(* Test avec profondeur donnée*)


let res_l = limit_klotski prof Klotski.anotherKlotski

let _ = oracle res_l

let _ = print_newline ()

(* Meta *)

let _ = Meta.meta_flag


(* IDS *)

let _ = IDS.ids_flag
 
let _ = print_endline ("Début Klotski IDS (max " ^ string_of_int prof ^ ")")

module I = IDS.Solver (Klotski.KlotskiProblem)
let ids_klotski n p = I.solver n Klotski.moves Klotski.isSolved p

(* Test avec profondeur max *)
let res_i = ids_klotski prof Klotski.anotherKlotski


let _ = oracle res_i

let _ = print_newline ()


(* Fin *)
let _ = print_endline "Fin des tests"
