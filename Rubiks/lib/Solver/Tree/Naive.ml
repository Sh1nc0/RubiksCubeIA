(**  A Simple Backtracking Inference Engine à la Prolog *)
open StateProblem

(** {5 Functor} *)

module Solver = functor (SP:STATEPROBLEM) ->
   
  struct

(**
{b Module     :}  A Simple Backtracking Inference Engine à la Prolog

{b Description:}  

{b Copyright  :}  (c) February 28, 2019, José Martinez, École polytechnique de l'université de Nantes

{b License    :}  AllRightsReserved

{b Maintainer :}  --

{b Stability  :}  Experimental, Linux

{b Portability:} OCaml 4.14.0

This is a /very basic/ backtracking solver.
Indeed, it it the simplest inference engine, based on a naïve /depth-first search/.

It is generally not to be used!
It can be safely used only for /finite/, /directed acyclic/, and /sparse/ state graphs ...

It presents a high risk of entering a false infinite search in the presence of directed cycles.
It can be used effectively only for problems, the graph of which is (mostly) acyclic, both directed and undirected.
For a graph with undirected cycles, there is a generally a super-exponential increase in the number of visited states.
Each state is visited through each path that leads to it from the initial state.
In particular, it founds /all/ the solutions, including the ones that are extensions of previous ones.
In order to avoid that, one has to use the "Solver.DirectedCycleDetection" variant.

The situation is even worse with infinite graphs, even locally finite, since it will search infinitely when it enters a sterile branch of the search space.
In order to avoid that, one has to use the "Solver.DepthLimit" variant.

For all these reasons, this algorithm is /not/ tested on random instances!
 *)


open SP

(**
A naïve backtracking solver /à la/ Prolog.

{b Post-condition. }

    - For any solution, the sequence of steps is indeed a solution.

{b Time complexity.}  Depends on the problem.  Generally exponential!
@param successors a transformation function that generates all the successors of a given state 
@param isFinalState the predicate that determines if a state is a final state 
@param i an initial state 
@return a list of solutions, each one being a list of transformations to apply in sequence, starting from the initial state, in order to reach a final state, i.e., solve the problem at hand*)
let solver successors isFinalState = 
  let rec go i =
    if isFinalState i 
    then [[]] (* already a final state, therefore the single solution is the state itself without any operation to apply *)
    else
      let suc = successors i in 
      List.flatten 
        (List.map 
           (fun j -> List.map (fun res -> j::res) (go (j.endState)))
           suc)
  in go



(**
A simple wrapper around [solver] dealing with the transformation of an [extension_state_problem] into the corresponding expected functions.
@param g a problem 
@return a list of lists of transformations, each one to apply in sequence from the initial state in order to reach a final state, i.e., solve the problem at hand
*)
let solver' g = 
  let p' = SP.extension2comprehension g
  in solver p'.successors p'.isFinalState g.theInitialState
   
end
