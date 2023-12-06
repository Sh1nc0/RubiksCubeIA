let _ = print_endline "Début des tests."

let _ = Rubiks.rubiks_flag


let moves = Rubiks.moves Rubiks.shuffled_cube

(* print sizes of moves *)
let length = List.length moves
let _ = print_endline ("Taille de la liste des mouvements :" ^ string_of_int length)


(* print moves *)

(* print moves *)


let oracle r = 
  if r = []
  then print_endline "Pas de solution trouvée." 
  else
    List. iter (fun s -> print_endline (Rubiks.RubiksProblem.short_solution s)) r


let prof = 4
(* let _ = print_endline ("Début Rubiks problem Limit (" ^string_of_int prof^ ")")

module DL = DepthLimit.Solver (Rubiks.RubiksProblem)

let limit_rubiks n p = DL.solver n Rubiks.moves  Rubiks.isSolved p

let res_l = limit_rubiks prof Rubiks.shuffled_cube *)

(* let _ = Rubiks.pretty_print Rubiks.shuffled_cube.unState *)



module I = IDS.Solver (Rubiks.RubiksProblem)
let ids_rubiks n p = I.solver n Rubiks.moves Rubiks.isSolved p

let _ = print_endline ("Début Rubiks problem IDS (" ^string_of_int prof^ ")")
let _ = Rubiks.pretty_print Rubiks.shuffled_cube.unState
let res_l = ids_rubiks prof Rubiks.shuffled_cube


let _ = oracle res_l

let _ = print_newline () 

