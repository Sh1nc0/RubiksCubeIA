(** A Backtracking Inference Engine with Depth Limit but Iterated Deepening *)

(**


  
{b Copyright  :}  (c) March 12, 2019, July 15, 2022, José Martinez, École polytechnique de l'université de Nantes

{b License    :}  AllRightsReserved

{b Maintainer :}  Julien Cohen for the OCaml version. 

{b Stability  :}  Experimental, Linux


{b Portability:} OCaml 4.14

The {!DepthLimit} has been introduced in order to avoid entering an infinite search in an infinitely deep search space.
However, using it directly is hard since we have to provide a maximal depth.
For instance, it can find lengthy solution before much shorter ones because it entered some sub-space before a more promising one.
By using it through "Solver.MetaIteratedDeepening" ({! Meta}), we avoid this problem;  the shorter solutions will be found sooner.
This solver is often named IDS for Iterated Deepening (Best-first) Search.
Its graph-based counterpart is the /breath-first/ search algorithm.
 *)

open StateProblem
open Meta

(** {5 Functor} *)

module Solver = functor (SP : STATEPROBLEM) ->
             struct 

(** A Backtracking Inference Engine with Depth Limit but Iterated Deepening (parameterized by [SP]).*)

module M = META(SP)
module DL = DepthLimit.Solver(SP)


(**  A meta backtracking solver with a depth limit, hence avoiding infinite searches as well as ensuring the minimal solutions, in the number of transformations, not the cost itself, are found.

 {b Post-condition.}

    - For any solution, the sequence of transformations is indeed a solution.

    - No solution can be of length greater than the limit.

    - Solutions appear in non-decreasing lengths.

@param dMax the maximal depth at which the search can be conducted from the initial state
@param successors a transformation function that generates all the successors of a given state
@param isFinalState the predicate that determines whether a state is a final state

@param i an initial state

@return a list of solutions, each one being a list of transformations to apply in sequence from the initial state in order to reach a final state, i.e., solve the problem at hand*)
let solver dMax successors isFinalState i= 
  M.metaSolver_first dMax (fun dMax -> DL.solver dMax successors isFinalState) i



(**
A simple wrapper around 'solver' dealing with the transformation of an 'ExtensionStateProblem' into the corresponding expected functions.

@param  dMax  the maximal depth at which the search can be conducted from the initial state
   @param g  a /finite/ problem in extension
   @return a list of lists of transformations, each one to apply in sequence from the initial state in order to reach a final state, i.e., solve the problem at hand*)
let solver' dMax g = 
  let p = SP.extension2comprehension g  in
  solver dMax (p.successors) (p.isFinalState) (g.theInitialState)
  
end

let ids_flag = true
