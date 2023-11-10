(* {1 Library of solvers for artificial intelligence project} *)

(* {!indexlist} *)

(* {b Author:} José Martinez, translated to OCaml by Julien Cohen.  *)


(* First, the problem to be considered has to be explessed in the term of the {!StateProblem} module. 

That module gives a module signature {!StateProblem.STATEPROBLEM} and a functor {!StateProblem.StateProblem} which helps to builds such a module.


Then, you can use different solvers available : {!Naive}, {!DepthLimit}, {!IDS} (more solvers in the Haskell version, the translations can be done on demand). These modules are provided as functors that take a {!StateProblem.STATEPROBLEM} module as parameter. *)

