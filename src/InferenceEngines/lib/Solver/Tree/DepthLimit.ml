(** A Backtracking Inference Engine with Depth Limit *)

(**


{b Copyright  :}  (c) March 1st, 2019, July 15, 2022, José Martinez, École polytechnique de l'université de Nantes

{b License    :}  AllRightsReserved

{b Maintainer :} Julien Cohen for the OCaml version

{b Stability  :}  Experimental, Linux

{b Portability:} OCaml 4.14

A backtracking solver with a depth limit is a simple extension of the naïve version, i.e., "Solver.Naive".

The naïve backtracking solver based on a /depth-first/ search is doomed to fail on infinite graphs whenever it enters a sterile sub-space of the search graph.
This translates into a never ending program, better say a crashing program when the memory has been exhausted.

The simple solution to address the problem of infinite graphs is to introduce a depth limit.

It can be used both for /infinite/ state graphs and graphs with directed cycles.
In other words, it can be used effectively to conduct searches /on any kind/ of state graph.

However, it is not expected to be effective nor efficient:

   - Avoiding infinite searches may also prohibit finding deep solutions.

   - In addition, should the depth limit be chosen too small, it will miss solutions.
   *)

open StateProblem

(** {5 Functor} *)

module Solver = functor (SP:STATEPROBLEM) -> struct

open SP

(**
 A basic backtracking solver with a depth limit, hence no risk of entering an infinite search.

{b Nota.}
    When the limit is set to a very high value, it behaves essentially like the naïve version.
    Conversely, when the limit is set too low, it may miss solutions.
    Determining the right value is problem-dependent, based either on empirical knowledge or known mathematical properties.

 {b Post-condition.}

    - For any solution, the sequence of transformations is indeed a solution.

    - No solution can be of length greater than the limit.
    
@param dMax the maximal depth at which the search can be conducted from the initial state
@param successors a transformation function that generates all the successors of a given state
@param isFinalState the predicate that determines if a state is a final state
@param i an initial state
@return a list of solutions, each one being a list of transformations to apply in sequence from the initial state in order to reach a final state, i.e., solve the problem at hand*)
let solver dMax successors isFinalState = 

  let rec go dMax i =
    if dMax < 0    
    then []
    else 
      if isFinalState i 
      then [[]]
      else
        let js = successors i
        in List.flatten 
             (List.map 
                (fun j -> List.map (fun ts -> j::ts) (go (dMax - 1) (j.endState))) 
                js)
  (*         [ j : ts
         | j <-   successors i
         , ts <-  go (dMax - 1) (endState j)
         ] *)     
  in 
  go dMax 



(**
 A simple wrapper around 'solver' dealing with the transformation of a 'ExtensionStateProblem' into the corresponding expected functions.


@param dMax  the maximal depth at which the search can be conducted from the initial state
@param p a /finite/ problem in extension
@return a list of lists of transformations, each one to apply in sequence from the initial state in order to reach a final state, i.e., solve the problem at hand
*)
let solver' dMax p = 
  let p' = extension2comprehension p
  in solver dMax
       (p'.successors)
       (p'.isFinalState)
       (p.theInitialState)


end
