let _ = print_endline "Début des tests."

let _ = Rubiks.rubiks_flag


let moves = Rubiks.moves Rubiks.shuffled_cube

let _ = print_endline ("****** Début des tests de Rubiks ******")

let oracle r =
  if r = []
  then print_endline "Pas de solution trouvée." 
  else
    List. iter (fun s -> print_endline (Rubiks.RubiksProblem.short_solution s)) r


let prof = 4

let _ = print_endline ("*** Début Rubiks problem Limit (" ^string_of_int prof^ ") ***")

let _ = print_endline ("** Shuffle by 2 up 90 and right -90 **")

module DL = DepthLimit.Solver (Rubiks.RubiksProblem)
let limit_rubiks n p = DL.solver n Rubiks.moves  Rubiks.isSolved p

let _ = Rubiks.pretty_print Rubiks.simple_cube.unState

let res_l = limit_rubiks prof Rubiks.simple_cube
let _ = oracle res_l
let _ = print_newline ()

let _ = print_endline ("** Shuffle by 3 **")

let limit_rubiks n p = DL.solver n Rubiks.moves  Rubiks.isSolved p
let _ = Rubiks.pretty_print Rubiks.shuffled_cube.unState

let res_l = limit_rubiks prof Rubiks.shuffled_cube
let _ = oracle res_l
let _ = print_newline ()


module I = IDS.Solver (Rubiks.RubiksProblem)
let ids_rubiks n p = I.solver n Rubiks.moves Rubiks.isSolved p

let _ = print_endline ("*** Début Rubiks problem IDS (" ^string_of_int prof^ ") ***")

let _ = print_endline ("** Shuffle by 3 **")
let _ = Rubiks.pretty_print Rubiks.shuffled_cube.unState
let res_l = ids_rubiks prof Rubiks.shuffled_cube

let _ = oracle res_l
let _ = print_newline () 


let _ = print_endline ("** Shuffle by 5 **")
let _ = Rubiks.pretty_print Rubiks.shuffled_cube2.unState
let res_l = ids_rubiks prof Rubiks.shuffled_cube2

let _ = oracle res_l
let _ = print_newline () 
