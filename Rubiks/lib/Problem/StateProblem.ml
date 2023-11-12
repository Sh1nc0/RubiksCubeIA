(** Formalisation of State Problems to be Solved by Inference Engines *)

open Additive


(**


{b Copyright  :}  (c) February 28, 2019, September 7, 2023, José Martinez, École polytechnique de l'université de Nantes

{b License    :}  AllRightsReserved

{b Maintainer :}  Julien Cohen for the Ocaml version

{b Stability  :}  Experimental, Linux

{b Portability:} OCaml 4.14.0 / 5.1.0


In Artificial Intelligence, a generic search problem in a state graph can be formalised as follows:
 
{ul
  {- A 'StateProblem' is defined in any way that best fits the problem to solve, from a single value, e.g., a string, to a complex "database" of facts and unknowns.}
  {- An initial state can be defined, though this is only one possible instance of a class of problems to solve.}
  {- In contrast, 'finalStates' must exist for the problem to be possibly solvable, indirectly defined thanks to the 'isFinalState' predicate.}
  {- Of most importance is the definition of the directed edges between states through the multi-valued 'successors' function, returning both:{ul  
  {- the list of successor states, along with}
  {- their label that consists of:{ul
            {- a description, and}
            {- a cost.}}}}}}

Solving a problem is nothing more than looking for a path in a labelled graph from an initial state to some final one, if it exists and if it is reachable!

The main point is that this graph can be /infinite/.
Even when it is not infinite, it can be of an /exponential/ size.
Therefore, it is defined in a functional way, i.e., in /comprehension/, rather than provided as a value, i.e., as an extension of nodes and edges.

{b Nota. }
   For testing purposes, it is possible to transform a value into functions.
   This is the role of the function 'comprehension2extension' of this module.
   Conversely, should the graph be finite, it is possible to rebuild its value by actually traversing all the nodes and edges.
   If infinite, it is still possible to rebuilt a (partial) sub-graph.
   This is the role of the companion function 'extension2comprehension'.
   In fact, this is nothing more than the role of all the search algorithms:  build a /minimal partial sub-graph/ of the whole graph that still exhibits a good solution!


*)

(** {2 Signatures of Parameter Modules} *)


module type STATE =
  sig
   type state
   val string_of_state : state -> string
  end

module type OPERATION =
  sig
   type op
   val string_of_op : op -> string
  end


(** {2 Signature of STATEPROBLEM Modules} *)

(** The module signature [STATEPROBLEM] is a common interface for problems to be solved by different solvers. *)
module type STATEPROBLEM =
  sig
    
(** This module signature is a common interface for problems to be solved by different solvers. *)

(** {5 Base types } *)

    type state 
    type op 
    type num 


(** {5 Transitions} *)

(**
A transformation is a triplet consisting of the /image/ of some state through a given operation.
This encodes:

- the description of the actual multi-valued function, [operation],

- one of the corresponding images, [state], as well as

- an accompanying function that provides the [cost] of the operation between the antecedent and the image states.

{b Nota.}
    Either the [startState] or [endState] attribute may seem unnecessary.
    Although slightly redundant, in most cases it is better to have them both rather than not.
    This avoids some additional parameter, or some zipping.
    In fact, since a transition is an /edge/ on a graph, it "naturally" contains its origin /and/ its destination.
*)
type transition  = 
   { startState : state ;
     operation  : op ;
     endState   : state ;
     cost       : num
   }


(**
Determining the successors of a node is the basic step for solving a problem.
In a state-based approach, they represent the directed edges from an origin state to a destination state.
The edge is labelled by a description of the applied transformation (i.e., the actual underlying function) as well as the cost of applying it.


{b Nota.}
    A single transformation encompasses several aspects:

- through the descriptions, we have a kind of curried view of a function,

- most often, functions do have pre-conditions, which remain implicit and hidden in the function itself here,

- functions do have post-conditions too, especially for limited the exploration by removing dead-ends, which can be applied by composing the generation and the filtering,

- the cost is a by-product of applying a transformation and is better formalised as an independent function.

{b Nota.}
    Also, as a possible post-condition, the function could avoid to generate successors from dead-end states, i.e., states for which we are sure that no final state is attainable.
    Alternatively, these dead-end states could be totally avoided from their parent successors.
    However, this two techniques somewhat interfere with the solvers that could plainly ask for this kind of property.
*)
type  successors  = state -> transition list

(** {5 Problem} *)

(**
 A problem to solve through a graph-based approach consists mainly of two functions:

- a function (representing formally a set of functions) that allows to generate new states from known ones,

- a predicate that determines whether a given state satisfies the solution requirements.

{b Nota.}
    The transformation function is described in detail through its dedicated type 'Successors'.

{b Nota.}
    A functional description of a problem is the only one feasible for infinite graphs, or even only large graphs.

 This data type is /not/ essential.
 In fact, it is barely used in practice!
 It appears in relationship with its extensional variant, 'ExtensionStateProblem', which is an actual graph.
 What is to be used are its associated data types:  'Transition', 'Successors', and 'Solution'.
 (Themselves could even be represented as mere tuples and lists of tuples ...)
*)
type state_problem = 
  { successors : successors  (** the graph is /implicitly/ described thanks to transformations from any given state to its successors*)
   ; isFinalState : state -> bool        (** the final states form a subset of 'states', described in comprehension thanks to a predicate *)
   }



(**
Being represented by a graph, in finite cases a problem can be fully provided (or generated).
It is a mere labelled graph!

 This form is also provided for testing purposes.

{b Nota.}
    An additional information is provided.
    It is some initial state.
    In other words, we are dealing with a problem /instance/ rather than a problem /class/.

 {b Constraints.}

    - The states must be comparable by equality.

    - The costs must be numeric values.

    - The three generic types must be "showable."
*)
type extension_state_problem 
   = 
   { states          : state list   (** all the known nodes of the state graph *)
   ; theInitialState : state              (** the initial state belongs to 'states' *)
   ; finalStates     : state list            (** the final states form a subset of 'states' *)
   ; edges           : (state * op * state * num) list (** edges are labelled both by a description and a cost (possibly uniformly unitary) *)
   }



(**
A smart constructor that checks that the following preconditions are verified.

 {b Pre-condition.}

    - The initial state is a member of the states.
    - The final states are included in the states.
    - The origin and destination states of the edges are included in the states.

 {b Time complexity.}

    - $O(m.n)$ where $n$ is the number of states and $m$ is the sum of the number of nodes in the final states as well as on both side of the edges plus the initial state.

    - $O(n^2)$ for a complete graph.

@param ss the states of the graph given in extension 
@param i some initial state of the graph 
@param fs the final states in the graph
@param es a set of labelled edges with a description and a cost 

@return the corresponding graph should the pre-condition be fulfilled  

@raise Assert_failure if the pre-conditions are not verified

*)
val mkExtensionStateProblem : 
  ss:state list   
   -> i:state   
   -> fs:state list 
   -> es:(state * op * state * num) list  
   -> extension_state_problem 

(**
A predicate verifies that the constraints on the type 'ExtensionStateProblem' are satisfied.

@param _ a problem described in extension

@return true if, and only if, the initial as well as final states belong to the set of states, and all the edges link known states with a positive cost 
*)
val isExtensionStateProblem : 
  extension_state_problem 
   -> bool               



(** {5 Solutions} *)

(**
A solution is a path in the graph from some initial state to a final state, made of consecutive operations.

 Of course, these consecutive operations must be actually linked to each other, and the last state be actually a final state.
 This is checked by the 'isSolution' predicate.
*)
type solution = transition list


(**
This predicate verifies whether a claimed solution is indeed a solution with respect to a problem.
 For this to be true:

    - the last state of the expected solution must be a final state,

    - each couple of consecutive states must belong to the authorised transformations, and

    - the first transformation must be applicable to the initial state.

 {b Nota. }
    As a special case, an empty list is also a solution, should the initial state be already a solution.

 {b Nota. }
    It is possible to find several solution states on a solution path.
    It is not the goal of this predicate to impose the minimality of a solution.

@param t a transformation function that generates the successors of a given state 
@param f the predicate that determines if a state is a final state 
@param i the initial state to which it is presumed to apply 
@param ts a potential solution as a list of transformations 

@return true if, and only if, all the transformation can be applied in sequence from the initial state and end up in a final state 
*)
val isSolution : 
  t:(state -> transition list) -> 
  f:(state -> bool) ->
  i:state -> 
  ts:solution ->
  bool       

(**
 The cost of a solution is the sum of the costs of its transformations.

 {b Nota. }
    This is different from the cost of /finding/ a solution, which is to the number of generated states before reaching a solution.
@param _ a (supposed to be) solution 
@return  the overall cost as the sum of the costs of the transitions 

*)
val  solutionCost :
  solution 
  -> num     


(**
From a descriptive to a function view of the edges\/transformations of a problem.

 Thanks to currying, this function can be called only with its first parameter in order to return the function required by the solvers.

 {b Post-condition.}

    - The generated predicate is satisfied by all the final states of the problem.

    - Conversely, the non-final states do not satisfy the predicate.
@param  p a generic problem 
@return the function describing the transformations of p (USE PARTIAL APPLICATION TO GET THAT FUNCTION).
*)
val childrenStates : 
  p:extension_state_problem 
   -> (state -> transition list)   


(**
 From a descriptive to a predicative view of the final states of a problem.

 {b Post-condition.}

    - The generated predicate is satisfied by all the final states of the problem.

    - Conversely, the non final states do not satisfy the predicate.
@param p a generic problem 
@return the predicate determining if the state is a final one (USE PARTIAL APPLICATION TO GET THAT FUNCTION).
*)
val isAFinalState : 
  extension_state_problem  
   -> (state -> bool)   



(** {5 Progress} *)

(**
 Development of (part of) a graph from a functional description.

 {b Nota. }
    The graph is created in a breadth-first search way.

 {b Time complexity.}  O(n^2).

@param n maximal number of fully developed nodes to create 
@param t the transformation function describing the transformations of p 
@param f the predicate determining if the state is a final one 
@param i the initial state 
@param ss the list of found nodes with followed out-going edges 
@param os the list of travelled edges
@param q a queue managed efficiently as a double (difference) list 
@return both the fully developed nodes and the solely reached ones along with their edges 

*)
val build :
  n:int 
   -> t:successors     
   -> f:(state -> bool)           
   -> i:state         
   -> ss:state list        
   -> os:(state * op * state * num) list    
   -> q:(state list * state list)      
   -> (state list) * (state * op * state * num) list

(** {5 Conversions} *)

(**
 The extensional (data) and intentional (functional) views of a problem are almost equivalent.
 They are equivalent if, and only if, the graph is small enough, i.e., finite, to be fully generated in its extensional version.
 In contrast, any data description can be transformed into a functional version.
@param g a problem described in extension as a labelled graph 
@return an equivalent version of the parameter described in comprehension. 
*)
val extension2comprehension : 
  extension_state_problem 
   -> state_problem  


(**
 From a functional view to a descriptive view.

 {b Nota. }
    An additional parameter is given in order to avoid generating infinite graphs.
    It does not corresponds to the maximal number of nodes to create but rather (implementation-dependent) to the number of fully treated nodes, i.e., those for which all the children have been also developed.

 {b Pre-condition.}

    - The number of generated nodes is at least one, for the initial state.

    - The number of generated nodes varies from one to slightly more than n, depending on the size of the underlying graph but also it connectedness.
      Therefore, n is only a crude value given to this generator...
@param n some (almost) maximal number of nodes to create 
@param t the transformation function describing the transformations of p 
@param f the predicate determining if the state is a final one
@param i the initial state 
@return the corresponding descriptive problem instance.
*)
val comprehension2extension :
  n:int 
   -> t:(state -> transition list)   
   -> f:(state -> bool)               
   -> i:state      
   -> extension_state_problem 


(** {5 Conversions to [string]} *)

val string_of_state : state -> string
val string_of_op : op -> string
val string_of_transition : transition -> string
val string_of_solution : solution -> string
val short_solution : solution -> string


end

(** {2 Functor} *)

(** The functor [StateProblem] takes three modules that describe basic types and builds a module with the convenient signature [STATEPROBLEM] to be used with solvers. *)
module  StateProblem (S:STATE) (D:OPERATION) (C:ADDITIVE) : STATEPROBLEM 
        with type state = S.state 
        with type op = D.op
        with type num = C.c =
  struct
    

(** {5 Base types } *)

    type state = S.state
    type op = D.op
    type num = C.c

(** {5 Transitions} *)

(**
 A transformation is a triplet consisting of the /image/ of some state through a given operation.
 This encodes:

    - the description of the actual multi-valued function, 'operation',

    - one of the corresponding images, 'state', as well as

    - an accompanying function that provides the 'cost' of the operation between the antecedent and the image states.

{b Nota}
    Either the 'startState' or 'endState' attribute may seem unnecessary.
    Although slightly redundant, in most cases it is better to have them both rather than not.
    This avoids some additional parameter, or some zipping.
    In fact, since a transition is an /edge/ on a graph, it "naturally" contains its origin /and/ its destination.
*)
type transition 
   = 
   { startState : state ; 
     operation  : op  ; 
     endState   : state ;
     cost       : num
   }



(**
 Determining the successors of a node is the basic step for solving a problem.
 In a state-based approach, they represent the directed edges from an origin state to a destination state.
 The edge is labelled by a description of the applied transformation (i.e., the actual underlying function) as well as the cost of applying it.


{b Nota}
    A single transformation encompasses several aspects:

       - through the descriptions, we have a kind of curried view of a function,

       - most often, functions do have pre-conditions, which remain implicit and hidden in the function itself here,

       - functions do have post-conditions too, especially for limited the exploration by removing dead-ends, which can be applied by composing the generation and the filtering,

       - the cost is a by-product of applying a transformation and is better formalised as an independent function.

{b Nota}
    Also, as a possible post-condition, the function could avoid to generate successors from dead-end states, i.e., states for which we are sure that no final state is attainable.
    Alternatively, these dead-end states could be totally avoided from their parent successors.
    However, this two techniques somewhat interfere with the solvers that could plainly ask for this kind of property.
*)
type successors  = state -> transition list


(** {5 Problem} *)

(**
 A problem to solve through a graph-based approach consists mainly of two functions:

    - a function (representing formally a set of functions) that allows to generate new states from known ones,

    - a predicate that determines whether a given state satisfies the solution requirements.

{b Nota}
    The transformation function is described in detail through its dedicated type 'Successors'.

{b Nota}
    A functional description of a problem is the only one feasible for infinite graphs, or even only large graphs.

 This data type is /not/ essential.
 In fact, it is barely used in practice!
 It appears in relationship with its extensional variant, 'ExtensionStateProblem', which is an actual graph.
 What is to be used are its associated data types:  'Transition', 'Successors', and 'Solution'.
 (Themselves could even be represented as mere tuples and lists of tuples ...)
*)
type state_problem = 
   { successors : successors  (** the graph is /implicitly/ described thanks to transformations from any given state to its successors*)
   ; isFinalState : state -> bool        (** the final states form a subset of 'states', described in comprehension thanks to a predicate *)
   }



(**
 Being represented by a graph, in finite cases a problem can be fully provided (or generated).
 It is a mere labelled graph!

 This form is also provided for testing purposes.

{b Nota}
    An additional information is provided.
    It is some initial state.
    In other words, we are dealing with a problem /instance/ rather than a problem /class/.

{b Constraints}

    - The states must be comparable by equality.

    - The costs must be numeric values.

    - The three generic types must be "showable."
*)
type extension_state_problem 
   = 
   { states          : state list ;  (** all the known nodes of the state graph *)
    theInitialState : state ;             (** the initial state belongs to 'states' *)
    finalStates     : state list ;            (** the final states form a subset of 'states' *)
    edges           : (state * op * state * num) list ; (** edges are labelled both by a description and a cost (possibly uniformly unitary) *)

   }

(**
 A smart constructor is provided, which is a better solution than the default instance for initialising a graph.

{b Pre-condition}

    - The initial state is a member of the states.
    - The final states are included in the states.
    - The origin and destination states of the edges are included in the states.

{b Time complexity}

    - $O(m.n)$ where $n$ is the number of states and $m$ is the sum of the number of nodes in the final states as well as on both side of the edges plus the initial state.

    - $O(n^2)$ for a complete graph.

@param ss the states of the graph given in extension 

@param i some initial state of the graph 

@param fs the final states in the graph

@param es a set of labelled edges with a description and a cost 

@return the corresponding graph should the pre-condition be fulfilled  

*)
let mkExtensionStateProblem ~ss ~i ~fs ~es =
   assert (List.mem i ss) ;
   assert (List.for_all (fun s -> List.mem s ss) fs) ; 
   assert (List.for_all (fun (o, _, d, _) -> List.mem o  ss && List.mem d ss ) es) ;
   { states = ss ; 
     theInitialState = i ; 
     finalStates = fs ; 
     edges = es }

(**
 A predicate that verifies that the constraints on the type 'ExtensionStateProblem' are satisfied.
@param p a problem described in extension

@return true if, and only if, the initial as well as final states belong to the set of states, and all the edges link known states with a positive cost 

*)
let isExtensionStateProblem p =
   List.mem p.theInitialState  p.states &&
   List.for_all (fun s -> List.mem s p.states) (p.finalStates) &&
   List.for_all (fun (s, _, e, c) ->
       List.mem s  p.states &&
         List.mem e  p.states &&
           C.strictly_positive c ) 
     p.edges


(** {5 Solutions} *)

(**
 A solution is a path in the graph from some initial state to a final state, made of consecutive operations.

 Of course, these consecutive operations must be actually linked to each other, and the last state be actually a final state.
 This is checked by the 'isSolution' predicate.
*)
type solution  = transition list


(**
 This predicate verifies whether a claimed solution is indeed a solution with respect to a problem.
 For this to be true:

    - the last state of the expected solution must be a final state,

    - each couple of consecutive states must belong to the authorised transformations, and

    - the first transformation must be applicable to the initial state.

{b Nota}
    As a special case, an empty list is also a solution, should the initial state be already a solution.

{b Nota}
    It is possible to find several solution states on a solution path.
    It is not the goal of this predicate to impose the minimality of a solution.

@param t a transformation function that generates the successors of a given state 
@param f the predicate that determines if a state is a final state 
@param i the initial state to which it is presumed to apply 
@param ts a potential solution as a list of transformations 

@return true if, and only if, all the transformation can be applied in sequence from the initial state and end up in a final state 

*)
let isSolution ~t ~f ~i ~ts =
  match ts with
   | [] -> f i
   | e::_ ->
      List.mem e (t i) 
      && f ((Miscellaneous.last ts).endState) 
      && List.for_all 
           (fun (i,j) -> List.mem j (t (i.endState))) 
           (Miscellaneous.consecutives ts)

(**
 The cost of a solution is the sum of the costs of its transformations.

{b Nota}
    This is different from the cost of /finding/ a solution, which is to the number of generated states before reaching a solution.
@param s a (supposed to be) solution 
@return the overall cost os [s] as the sum of the costs of its transitions 

*)
let solutionCost s = 
  List.fold_right 
    C.add 
    (List.map (fun x -> x.cost) s) 
    C.zero 


(**
 From a descriptive to a function view of the edges\/transformations of a problem.

 Thanks to currying, this function can be called only with its first parameter in order to return the function required by the solvers.

{b Post-condition}

    - The generated predicate is satisfied by all the final states of the problem.

    - Conversely, the non-final states do not satisfy the predicate.
@param p a generic problem 
@param i DO NOT USE DIRECTLY, use partial application to get a function
@return the function describing the transformations of [p]

*)
let childrenStates ~p i =
  List.map 
    (fun (_,d,s,c) ->  { startState = i ; operation = d ; endState = s ; cost = c })
    (List.filter (fun (j,_,_,_) -> j = i) p.edges)


(**
 From a descriptive to a predicative view of the final states of a problem.

{b Post-condition}

    - The generated predicate is satisfied by all the final states of the problem.

    - Conversely, the non final states do not satisfy the predicate.

@param p a generic problem 
@param s DO NOT USE DIRECTLY, use partial application to get a predicate
@return  the predicate determining if the state is a final one

*)
let isAFinalState p = fun s -> List.mem s p.finalStates




(** {5 Progress} *)

(**
 Development of (part of) a graph from a functional description.

{b Nota}
    The graph is created in a breadth-first search way.

{b Time complexity} O(n^2).

@param n maximal number of fully developed nodes to create 
@param t the transformation function describing the transformations of p 
@param f the predicate determining if the state is a final one 
@param i the initial state 
@param ss the list of found nodes with followed out-going edges
@param os the list of travelled edges 
@param q a queue managed efficiently as a double (difference) list 

@return both the fully developed nodes and the solely reached ones along with their edges 

*)
let rec build  ~n ~t ~f ~i ~ss ~os ~q =
  match (n,q) with
  | (0,(qs,ps)) -> (Ord.nubOrd (i::ss @ qs @ ps), os)
  | (_,([],[])) -> (ss, os)
  | (_,([],ps)) -> build ~n ~t ~f ~i ~ss ~os ~q:(List.rev ps, [])
  | (_,(e::r,ps)) when List.mem i ss -> build ~n ~t ~f ~i:e ~ss ~os ~q:(r, ps)
  | (_,(e::r,ps)) -> 
      let cs = t i      
      and endState tr = tr.endState
      and operation i tr = (i, tr.operation, tr.endState, tr.cost)
      in
      build 
        ~n:(n - 1)
        ~t
        ~f
        ~i:e
        ~ss:(i :: ss)
        ~os:(List.map (operation i) cs @ os)
        ~q:(r, List.map endState cs @ ps)
      

(** {5 Conversions} *)

(**
 The extensional (data) and intentional (functional) views of a problem are almost equivalent.
 They are equivalent if, and only if, the graph is small enough, i.e., finite, to be fully generated in its extensional version.
 In contrast, any data description can be transformed into a functional version.
*)
let extension2comprehension g =
  { isFinalState = isAFinalState g ;
    successors   = childrenStates ~p:g
  }



(**
 From a functional view to a descriptive view.

 {b Nota}
    An additional parameter is given in order to avoid generating infinite graphs.
    It does not corresponds to the maximal number of nodes to create but rather (implementation-dependent) to the number of fully treated nodes, i.e., those for which all the children have been also developed.

{b Pre-condition}

    - The number of generated nodes is at least one, for the initial state.

    - The number of generated nodes varies from one to slightly more than n, depending on the size of the underlying graph but also it connectedness.
      Therefore, n is only a crude value given to this generator...

@param n some (almost) maximal number of nodes to create 
@param t the transformation function describing the transformations of p 
@param f the predicate determining if the state is a final one
@param i the initial state 
@return the corresponding descriptive problem instance

*)
let comprehension2extension ~n ~t ~f ~i =
   if n <= 0
      then failwith "The number of nodes must be strictly positive"
   else
     let (ss, os) = build ~n ~t ~f ~i ~ss:[] ~os:[] ~q:([i], []) 
     in
     { states          = ss ;
       theInitialState = i  ;
       finalStates     = List.filter f ss ;
       edges           = os ;
     }

(** {5 Conversions to [string]} *)

let string_of_state = S.string_of_state
let string_of_op = D.string_of_op
let string_of_cost = C.string_of_c

let string_of_transition s = "<" ^ string_of_state s.startState 
                             ^ " / " ^ string_of_op s.operation
                             ^ "->" ^ string_of_state s.endState
                             ^ " (" ^ string_of_cost s.cost 
                             ^ ")>"

let string_of_solution s = List.fold_right (fun t r -> string_of_transition t ^ r) s ""


let short_solution s = List.fold_right (fun t r -> string_of_op (t.operation) ^ ", " ^ r) s ""


end


