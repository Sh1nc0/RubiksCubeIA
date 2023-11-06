(** A Meta Backtracking Inference Engine with Depth Limit but Iterated Deepening *)
 


open StateProblem

(** {5 [META] functor } *)

module META = functor (SP : STATEPROBLEM)-> 
              struct

(** A Meta Backtracking Inference Engine with Depth Limit but Iterated Deepening

{b Copyright  :}  (c) March 12, 2019, July 15, 2022, José Martinez, École polytechnique de l'université de Nantes

{b License    :}  AllRightsReserved

{b Maintainer :} Julien Cohen for the OCaml version

{b Stability  :}  Experimental, Linux

{b Portability:}  OCaml 4.14.0

The naïve approach, i.e., "Solver.Naive", has an issue with /infinite/ graphs.
In order to address this main problems of the naïve approach, the depth-limited algorithm solver, i.e., "Solver.DepthLimit", introduces an /unnatural/ parameter.
It is difficult to tune the "max depth" hyper-parameter:

   - Should it be /too small/, solutions that exist would be missed.

   - Should it be /too large/, we could end up:
   {ul
        {- exploring large sterile branches, and\/or}
        {- finding largely sub-optimal solutions, as well as}
        {- spending a lot of time in the search.}}

The /iterated/ deepening approach module avoids these issues.
A maximal depth is still required.
However, it is no longer the maximal depth of a single search but the maximal depth of several searches, from a minimal depth of 0 up to the maximal depth, incrementing it after each restart, i.e., when no solution has been found at the previous tentative depth limit.

This solver is both /effective/ and rather /efficient/.

   - The first problem is solved since the solutions which are not deep will be found first.
     The second problem is solved for the same reason.
     Of course, the issue of infinite graph is solved by the depth parameter, as of "Solver.DepthLimit".
     Basically, it repeats searches at deeper and deeper levels in order to find a solution.
     Therefore, it becomes a kind of /breadth-first/ search.
     Hence, it is optimal with respect to the length of the solution (not necessarily its cost).
     Also, it is complete as long as the state graph is locally finite (i.e., no transformation generates an infinity of successor states).

   - This may seem very costly ...
     However, its complexity is /only and at most twice/ the complexity of the underlying search!
     Effectively, running iteratively an exponential algorithm at most doubles its time complexity, which is a very small multiplicative constant!
     Finally, its asymptotic time complexity is the same as the underlying depth-limited search.
     Therefore, it is a /very good candidate/ for conducting recursive searches.

However, this is a /meta/ algorithm.
It relies on any solver that uses the depth limit.
 *)


(**{b Translation to OCaml.} In the Haskell code of the library, if we only access to the first result in [metaSolver _ _ _], the minimal number of iterations to find a solution will be executed and the other iterations will not be executed, because of the laziness. If we want a solution with a better cost (and more transitions), we can read the following elements of the result, and then the computation will trigger the other iterations. 

In the Ocaml translation of the library, we provide two operations 

- stop at the first iteration where some solutions have been found. 

- stop at the first iteration where some solutions under a given cost have been found. 
*)


 
(**
 A meta backtracking solver with a depth limit, hence avoiding infinite searches as well as ensuring the minimal solution, in the number of transformations, not the cost itself, are found.

{b Post-condition.}

    - For any solution, the sequence of transformations is indeed a solution.

    - No solution can be of length greater than the limit.

    - Solutions appear in non-decreasing lengths.

{b Nota.}  These post-conditions have to be ensured by any specific implementation.
*)
(*
   :: (Eq s, Num c)
   => Int                             ^ dMax, 
   -> (Int -> s -> [Solution d s c])  ^ solver, 
   -> s                               ^ i, 
   -> [Solution d s c]                ^ tss, *)
  
(** {b Translation to OCaml.} In the Haskell code of the library, if we only access to the first result in [metaSolver _ _ _], the minimal number of iterations to find a solution will be executed and the other iterations will not be executed, because of the laziness. If we want a solution with a better cost (and more transitions), we can read the following elements of the result, and then the computation will trigger the other iterations. 

In the Ocaml translation, we stop at the first iteration where some solutions have been found. 
@param dMax the maximal depth at which the search can be conducted from the initial state
@param solver a solver that accepts a depth limit as its first parameter
@param i an initial state
@return a list of solutions, each one being a list of transformations to apply in sequence from the initial state in order to reach a final state, i.e., solve the problem at hand
*)
let metaSolver_first dMax solver i =
  let rec metaSolver_aux current_iteration =
    if current_iteration > dMax 
    then []
    else match solver current_iteration i with
         | [] -> metaSolver_aux (current_iteration +1)
         | r -> r
  in metaSolver_aux 0

(**
 A meta backtracking solver with a depth limit, hence avoiding infinite searches as well as ensuring the minimal solution, in the number of transformations, within a given cost, are found.

{b Post-condition.}

    - For any solution, the sequence of transformations is indeed a solution.

    - No solution can be of length greater than the limit.

    - Solutions appear in non-decreasing lengths.

{b Nota.}  These post-conditions have to be ensured by any specific implementation.
@param c the maximal acceptable cost 
@param dMax the maximal depth at which the search can be conducted from the initial state
@param solver a solver that accepts a depth limit as its first parameter
@param i an initial state
@return a list of solutions, each one being a list of transformations to apply in sequence from the initial state in order to reach a final state, i.e., solve the problem at hand

*)
let metaSolver_with_cost c dMax solver i =
  let rec metaSolver_aux current_iteration =
    if current_iteration > dMax 
    then []
    else 
      match List.filter (fun s -> SP.solutionCost s <= c (* FIXME : polymorphic comparison instead of Num comparison*)) (solver current_iteration i) 
      with
      | [] -> metaSolver_aux (current_iteration +1)
      | r -> r
  in metaSolver_aux 0

end

let meta_flag = true
