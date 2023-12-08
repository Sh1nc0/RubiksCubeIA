(** The Rubiks Puzzle *)
(**
{b Maintainer :} PIPON Romain & LEBRETON Thomas

{b Stability :} Experimental

{b Portability :} OCaml 4.14.0

{b Description :}
This module implements the Rubiks Puzzle. It is a 2x2x2 Rubik's cube.
This is not the most efficient and we get difficulties to go more than depth 4/5.
*)

(* ###### MARK: TYPES ######*)
(** [couleur] represents the colors of the cube. *)
type couleur = R of char | G of char | B of char | Y of char | O of char | W of char

(** [piece] represents a piece of the cube. *)
type piece = (couleur * int)

(** [face] represents the faces of the cube. *)
type face =
  UP of char
  | DOWN of char
  | LEFT of char
  | RIGHT of char
  | FRONT of char
  | BACK of char

(** [operation] represents an operation on the cube, (face, angle). *)
type operation = (face * int)

(** [r_state] represents the state of the Rubik's cube. *)
type r_state = {unState : piece list; lastOp : ((face * int) option)}


(* ###### MARK: FONCTIONS UTILES ######*)

(** {b Function to concatenate two lists}

{b Precondition :} Takes two non-empty lists

{b Postcondition :} Returns a concatenated list

{b Example :} cat ([1; 2; 3], [4; 5; 6]) = [1; 2; 3; 4; 5; 6]
*)
let rec cat = function
  ([], y) -> y
  | (x::xs, y) -> x :: cat (xs, y)

(** {b Function to skip elements in a list}

{b Precondition :} Takes a non-empty list and a positive integer

{b Postcondition :} Returns a list with the first n elements skipped

{b Example :} skip ([1; 2; 3; 4; 5], 2) = [3; 4; 5]
*)
let rec skip = function
  ([], _) -> []
  | (_::xs as xs1, c) -> if c > 0 then skip(xs, c - 1) else xs1

(** {b Function to take elements from the beginning of a list}

{b Precondition :} Takes a non-empty list and a positive integer

{b Postcondition :} Returns a list with the first n elements

{b Example :} take ([1; 2; 3; 4; 5], 2) = [1; 2]
*)
let rec take = function
  ([], _) -> []
  | (x::xs, c) -> if c > 0 then x :: take(xs, c - 1) else []

(** {b Function to cycle a list to the right}

{b Precondition :} Takes a non-empty list and a positive integer

{b Postcondition :} Returns a list with the first n elements at the end

{b Example :} cycle_right ([1; 2; 3; 4; 5], 2) = [4; 5; 1; 2; 3]
*)
let cycle_right (lst: 'a list) (n: int) =
  let len = List.length lst in
  let i = (len - (n mod len)) mod len in
  cat (skip (lst, i), take (lst, i))

(** {b Function to split a list at a given position}

{b Precondition :} Takes a non-empty list and a positive integer

{b Postcondition :} Returns a tuple of two lists, the first one containing the first n elements and the second one containing the rest

{b Example :} split ([1; 2; 3; 4; 5], 2) = ([1; 2], [3; 4; 5])
*)
let rec split (lst: 'a list) (n: int) : ('a list * 'a list) =
  if n = 0 then
    ([], lst)
  else
    match lst with
    | [] -> ([], [])
    | head :: tail ->
      let left, right = split tail (n - 1)  in
      (head :: left, right)


(* ###### MARK: FONCTIONS LIE AU RUBIKS ######*)

(** {b Function to convert a state to string}

{b Precondition :} Takes a state

{b Postcondition :} Returns a string representing the state

{b Example :} string_of_state {unState = [(W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4); (R 'r', 1); (R 'r', 2); (R 'r', 3); (R 'r', 4); (B 'b', 1); (B 'b', 2); (B 'b', 3); (B 'b', 4); (G 'g', 1); (G 'g', 2); (G 'g', 3); (G 'g', 4); (Y 'y', 1); (Y 'y', 2); (Y 'y', 3); (Y 'y', 4); (O 'o', 1); (O 'o', 2); (O 'o', 3); (O 'o', 4)]} = "wwwwrrrrbbbbggggyyyyoooo"
*)
let string_of_state { unState } =
  let rec string_of_state_aux = function
    | [] -> ""
    | (c, _) :: tail -> (match c with
      | R c | G c | B c | Y c | O c | W c -> String.make 1 c) ^ string_of_state_aux tail
  in
  string_of_state_aux unState

(** {b Function to convert an angle to string}

{b Precondition :} Takes an angle that is an integer in (-90, 90, 180)

{b Postcondition :} Returns a string representing the angle

{b Example :}

string_of_angle 180 = "2",

string_of_angle 90 = "",

string_of_angle (-90) = "'"
*)
let string_of_angle = function
| 90 -> ""
| -90 -> "'"
| 180 -> "2"
| _ -> raise (Failure "Angle invalide")


(** {b Function to convert a move to string}

{b Precondition :} Takes a move that is a tuple of a face and an angle that is an integer in (-90, 90, 180)

{b Postcondition :} Returns a string representing the move

{b Example :}

string_of_move (UP 'U', 90) = "U"

string_of_move (UP 'U', -90) = "U'"

string_of_move (DOWN 'D', 180) = "D2"
*)
let string_of_move (face , a) = match face with
  | UP pid -> Char.escaped pid ^ string_of_angle (a)
  | DOWN pid -> Char.escaped pid ^ string_of_angle (a)
  | LEFT pid -> Char.escaped pid ^ string_of_angle (a)
  | RIGHT pid -> Char.escaped pid ^ string_of_angle (a)
  | FRONT pid -> Char.escaped pid ^ string_of_angle (a)
  | BACK pid -> Char.escaped pid ^ string_of_angle (a)

(** One final state of the Rubik's cube*)
let etat_final : piece list = [
  (W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4); (*0, 1, 2, 3*)
  (R 'r', 1); (R 'r', 2); (R 'r', 3); (R 'r', 4); (*4, 5, 6, 7*)
  (B 'b', 1); (B 'b', 2); (B 'b', 3); (B 'b', 4); (*8, 9, 10, 11*)
  (G 'g', 1); (G 'g', 2); (G 'g', 3); (G 'g', 4); (*12, 13, 14, 15*)
  (Y 'y', 1); (Y 'y', 2); (Y 'y', 3); (Y 'y', 4); (*16, 17, 18, 19*)
  (O 'o', 1); (O 'o', 2); (O 'o', 3); (O 'o', 4); (*20, 21, 22, 23*)
]

(* {b Function to extract characters from the tuple and generate an array representing the problem }


{b Precondition :} Takes a list of pieces

{b Postcondition :} Returns a list of characters

{b Example :} extraire_caracteres [(W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4)] = ['w'; 'w'; 'w'; 'w']
*)
let extraire_caracteres (s : piece list) : char list =
  List.map (fun (couleur, _) ->
    match couleur with
    | R c | G c | B c | Y c | W c | O c -> c
  ) s

(** {b Function to check if the Rubik's cube is solved}


{b Precondition :} Takes a state

{b Postcondition :} Returns true if the cube is solved, false otherwise
*)
let isSolved {unState} = match extraire_caracteres unState with
  | ['w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'] -> true
  | ['r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'] -> true
  | ['b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'] -> true
  | ['g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'] -> true
  | ['y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'] -> true
  | ['o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'] -> true
  | _ -> false

(* ###### MARK: PROBLEM ######*)

(** Building a state problem *)
module RubiksProblem = 
( StateProblem.StateProblem
  (struct
    type state = r_state
    let string_of_state  = string_of_state
  end)
  (struct
    type op = operation
    let string_of_op = string_of_move
  end)
  (Additive.IntC)
)

(** {b Function to get the indices of the faces that need to be rotated}

{b Precondition :} Takes a face in (UP, DOWN, LEFT, RIGHT, FRONT, BACK)

{b Postcondition :} Returns a list of indices

{b Example :} indice_rotation (UP 'U') = [0; 1; 2; 3; 7; 6; 8; 11; 13; 12; 18; 17]
*)
let indice_rotation (face: face) : int list = match face with
  | UP pid -> [0; 1; 2; 3; 7; 6; 8; 11; 13; 12; 18; 17]
  | DOWN pid-> [21; 20; 23; 22; 4; 5; 9; 10; 14; 15; 19; 16]
  | RIGHT pid -> [11; 8; 9; 10; 2; 1; 6; 5; 20; 23; 14; 13]
  | LEFT pid -> [17; 18; 19; 16; 0; 3; 12; 15; 22; 21; 4; 7]
  | FRONT pid -> [12; 13; 14; 15; 3; 2; 11; 10; 23; 22; 19; 18]
  | BACK pid -> [6; 7; 4; 5; 1; 0; 17; 16; 21; 20; 9; 8]

(** {b Function to extract the pieces that need to be rotated}

{b Precondition :} Takes a list of indices and a list of pieces

{b Postcondition :} Returns a list of pieces

{b Example :} extraire_pieces [0; 1; 2; 3; 7; 6; 8; 11; 13; 12; 18; 17] [(W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4); (R 'r', 1); (R 'r', 2); (R 'r', 3); (R 'r', 4); (B 'b', 1); (B 'b', 2); (B 'b', 3); (B 'b', 4); (G 'g', 1); (G 'g', 2); (G 'g', 3); (G 'g', 4); (Y 'y', 1); (Y 'y', 2); (Y 'y', 3); (Y 'y', 4); (O 'o', 1); (O 'o', 2); (O 'o', 3); (O 'o', 4)] = [(W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4); (R 'r', 1); (R 'r', 2); (R 'r', 3); (R 'r', 4); (B 'b', 1); (B 'b', 2); (B 'b', 3); (B 'b', 4)]
*)
let rec extraire_pieces (indices: int list) (s: piece list) : piece list=
  match indices with
  | [] -> []
  | i :: tail -> (List.nth s i) :: extraire_pieces tail s

(** {b Function to rotate the pieces}


{b Precondition :}  Takes an angle that is an integer in (-90, 90, 180) and a list of pieces

{b Postcondition :} Returns a list of pieces rotated of the pieces needed to be rotated
*)
let rec rotation_angle (angle: int) (s: piece list) : piece list =
  match angle with
  | 90 ->
    let (r, l) = split s 4 in
    let rotate_r = cycle_right r 1 in
    let rotate_l = cycle_right l 2 in
    let j = cat(rotate_r, rotate_l) in
    j
  | -90 -> rotation_angle 90 (rotation_angle 90 (rotation_angle 90 s))
  | 180 -> rotation_angle 90 (rotation_angle 90 s)
  | _ -> raise (Failure "Angle invalide")

(** {b Function to rotate the cube}

{b Precondition :} Takes a face in (UP, DOWN, LEFT, RIGHT, FRONT, BACK), an angle that is an integer in (-90, 90, 180) and a state

{b Postcondition :} Returns a list of pieces rotated of all the pieces of the cube
*)
let rotation (face: face) (angle: int) (s: r_state) : piece list =
  let k = s.unState in
  let indices = indice_rotation face in
  let pieces = extraire_pieces indices k in
  let rotated_pieces = rotation_angle angle pieces in
  let rec remplacer_pieces indices pieces k =
    match indices, pieces with
    | [], [] -> k
    | i :: tail, p :: tail2 -> remplacer_pieces tail tail2 (List.mapi (fun index (c, _) -> if index = i then p else (c, index + 1)) k)
    | _ -> raise (Failure "Erreur de rotation")
  in
  remplacer_pieces indices rotated_pieces k

(** {b Function to generate the transitions}

{b Precondition :} Takes a state and the last operation

{b Postcondition :} Returns a list of transitions
*)
let moves {unState; lastOp} =
    let moves = [UP 'U'; DOWN 'D'; LEFT 'L'; RIGHT 'R'; FRONT 'F'; BACK 'B'] in
    let angles = [90; -90; 180] in

    let generate_transition_from_move angle move =
      let end_state = rotation move angle {unState = unState; lastOp = (Some (move, angle))} in
      { RubiksProblem.startState = {unState = unState; lastOp = (Some (move, angle))};
        operation = (move, angle);
        endState = {unState = end_state; lastOp = (Some (move, angle))};
        cost = 1;
      } in
      let generate_transition_for angle =
        List.map (generate_transition_from_move angle) moves 
      in
      List.flatten (List.map generate_transition_for angles)

(** {b Function to get the mirror move}

{b Precondition :} Takes a move

{b Postcondition :} Returns the mirror move

{b Example :} inverse_move (UP 'U', 90) = (UP 'U', -90)
*)
let inverse_move (move: (face * int)) : (face * int) =
  let (face, angle) = move in
  let new_angle = match angle with
    | 180 -> 180
    | 90 -> -90
    | -90 -> 90
    | _ -> raise (Failure "Angle invalide")
  in
  (face, new_angle)

(** {b Function du pretty print the cube}

{b Precondition :} Takes a list of pieces

{b Postcondition :} Prints the cube
*)
let pretty_print (s: piece list) : unit =
  let chars = extraire_caracteres s in
  let i index = (List.nth chars index) in
  Printf.printf "    %c %c\n" (i 4) (i 5);
  Printf.printf "    %c %c\n" (i 7) (i 6) ;
  Printf.printf "%c %c %c %c %c %c %c %c\n" (i 16) (i 17) (i 0) (i 1) (i 8) (i 9) (i 20) (i 21);
  Printf.printf "%c %c %c %c %c %c %c %c\n" (i 19) (i 18) (i 3) (i 2) (i 11) (i 10) (i 23) (i 22);
  Printf.printf "    %c %c\n" (i 12) (i 13);
  Printf.printf "    %c %c\n" (i 15) (i 14);
  print_newline ()

(** {b Function to shuffle the cube}

{b Precondition :} Takes a state and an integer

{b Postcondition :} Returns a state shuffled n times
*)
let rec shuffle (s: r_state) (n: int) =
  let moves = [UP 'u'; DOWN 'd'; LEFT 'l'; RIGHT 'r'; FRONT 'f'; BACK 'b'] in
  let rotations = [90; -90; 180] in
  if n = 0 then s
  else
    let _ = Random.self_init () in
    let m = List.nth moves (Random.int 6) in
    let r = List.nth rotations (Random.int 3) in
    let s = rotation m r s in
    shuffle {unState = s; lastOp= Some (m, r)} (n - 1)


(* ###### MARKS: some cubes ###### *)

(** A simple cube suffled 2 time Up 90 and Righ -90 *)
let simple_cube = {unState = (rotation (RIGHT 'R') (-90) {unState = (rotation (UP 'U') 90 {unState = etat_final; lastOp=None}); lastOp=None}); lastOp=None}

(** A cube randomly shuffled by 2*)

(** A cube randomly shuffled by 3*)
let shuffled_cube = shuffle {unState = etat_final; lastOp=None} 3

(** A cube randomly shuffled by 5*)
let shuffled_cube2 = shuffle {unState = etat_final; lastOp=None} 5


let rubiks_flag = 1