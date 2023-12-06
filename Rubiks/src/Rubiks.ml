(** The Rubiks Puzzle *)

(* ###### MARK: TYPES ######*)
(* red, green, blue, yellow, orange, white *)
type couleur = R of char | G of char | B of char | Y of char | O of char | W of char

type piece = (couleur * int)

type face =
  UP of char
  | DOWN of char
  | LEFT of char
  | RIGHT of char
  | FRONT of char
  | BACK of char

type operation = (face * string)

type r_state = {unState : piece list; lastOp : ((face * int) option)}


(* ###### MARK: FONCTIONS UTILES ######*)

(* Function to concatenate two lists *)
let rec cat = function
  ([], y) -> y
  | (x::xs, y) -> x :: cat (xs, y)

(* Function to skip elements in a list *)
let rec skip = function
  ([], _) -> []
  | (_::xs as xs1, c) -> if c > 0 then skip(xs, c - 1) else xs1

(* Function to take elements from the beginning of a list *)
let rec take = function
  ([], _) -> []
  | (x::xs, c) -> if c > 0 then x :: take(xs, c - 1) else []

(* Function to cycle a list to the right *)
let cycle_right (lst: 'a list) (n: int) =
  let len = List.length lst in
  let i = (len - (n mod len)) mod len in
  cat (skip (lst, i), take (lst, i))

(* Function to split a list at a given position *)
let rec split (lst: 'a list) (n: int) : ('a list * 'a list) =
  if n = 0 then
    ([], lst)
  else
    match lst with
    | [] -> ([], [])
    | head :: tail ->
      let left, right = split tail (n - 1)  in
      (head :: left, right)
      let rec join l1 l2 =
  match l1 with
  | [] -> l2
  | head :: tail -> head :: join tail l2

(* ###### MARK: FONCTIONS LIE AU RUBIKS ######*)

(* Function to *)
let string_of_state { unState } =
  let rec string_of_state_aux = function
    | [] -> ""
    | (c, _) :: tail -> (match c with
      | R c | G c | B c | Y c | O c | W c -> String.make 1 c) ^ string_of_state_aux tail
  in
  string_of_state_aux unState

let string_of_move (face , a) = match face with
  | UP pid -> Char.escaped pid ^ a
  | DOWN pid -> Char.escaped pid ^ a
  | LEFT pid -> Char.escaped pid ^ a
  | RIGHT pid -> Char.escaped pid ^ a
  | FRONT pid -> Char.escaped pid ^ a
  | BACK pid -> Char.escaped pid ^ a

let string_of_angle = function
| 90 -> ""
| -90 -> "'"
| 180 -> "2"
| _ -> raise (Failure "Angle invalide")

let etat_final : piece list = [
  (W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4); (*0, 1, 2, 3*)
  (R 'r', 1); (R 'r', 2); (R 'r', 3); (R 'r', 4); (*4, 5, 6, 7*)
  (B 'b', 1); (B 'b', 2); (B 'b', 3); (B 'b', 4); (*8, 9, 10, 11*)
  (G 'g', 1); (G 'g', 2); (G 'g', 3); (G 'g', 4); (*12, 13, 14, 15*)
  (Y 'y', 1); (Y 'y', 2); (Y 'y', 3); (Y 'y', 4); (*16, 17, 18, 19*)
  (O 'o', 1); (O 'o', 2); (O 'o', 3); (O 'o', 4); (*20, 21, 22, 23*)
]

(* Function to extract characters from the tuple and generate an array representing the problem *)
let extraire_caracteres (s : piece list) : char list =
  List.map (fun (couleur, _) ->
    match couleur with
    | R c | G c | B c | Y c | W c | O c -> c
  ) s

(* Function to check if the Rubik's cube is solved *)
let isSolved {unState} = match extraire_caracteres unState with
  | ['w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'] -> true
  | ['r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'] -> true
  | ['b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'] -> true
  | ['g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'] -> true
  | ['y'; 'y'; 'y'; 'y'; 'o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'] -> true
  | ['o'; 'o'; 'o'; 'o'; 'w'; 'w'; 'w'; 'w'; 'r'; 'r'; 'r'; 'r'; 'b'; 'b'; 'b'; 'b'; 'g'; 'g'; 'g'; 'g'; 'y'; 'y'; 'y'; 'y'] -> true
  | _ -> false

(* ###### MARK: PROBLEM ######*)

(* Building a state problem *)
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

(* Function to get the indices of the faces that need to be rotated *)
let indice_rotation (face: face) : int list = match face with
  | UP pid -> [0; 1; 2; 3; 7; 6; 8; 11; 13; 12; 18; 17]
  | DOWN pid-> [21; 20; 23; 22; 4; 5; 9; 10; 14; 15; 19; 16]
  | RIGHT pid -> [11; 8; 9; 10; 2; 1; 6; 5; 20; 23; 14; 13]
  | LEFT pid -> [17; 18; 19; 16; 0; 3; 12; 15; 22; 21; 4; 7]
  | FRONT pid -> [12; 13; 14; 15; 3; 2; 11; 10; 23; 22; 19; 18]
  | BACK pid -> [6; 7; 4; 5; 1; 0; 17; 16; 21; 20; 9; 8]

let rec extraire_pieces (indices: int list) (s: piece list) : piece list=
  match indices with
  | [] -> []
  | i :: tail -> (List.nth s i) :: extraire_pieces tail s

let rec rotation_angle (angle: int) (s: piece list) : piece list =
  match angle with
  | 90 ->
    let (r, l) = split s 4 in
    let rotate_r = cycle_right r 1 in
    let rotate_l = cycle_right l 2 in
    let j = join rotate_r rotate_l in
    j
  | -90 -> rotation_angle 90 (rotation_angle 90 (rotation_angle 90 s))
  | 180 -> rotation_angle 90 (rotation_angle 90 s)
  | _ -> raise (Failure "Angle invalide")

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

let moves {unState; lastOp} =
    let moves = [UP 'U'; DOWN 'D'; LEFT 'L'; RIGHT 'R'; FRONT 'F'; BACK 'B'] in
    let angles = [90; -90; 180] in

    let generate_transition_from_move angle move =
      let end_state = rotation move angle {unState = unState; lastOp = (Some (move, angle))} in
      { RubiksProblem.startState = {unState = unState; lastOp = (Some (move, angle))};
        operation = (move, string_of_angle angle);
        endState = {unState = end_state; lastOp = (Some (move, angle))};
        cost = 1;
      } in
      let generate_transition_for angle =
        List.map (generate_transition_from_move angle) moves 
      in
      List.flatten (List.map generate_transition_for angles)

let inverse_move (move: (face * int)) : (face * int) =
  let (face, angle) = move in
  let new_angle = match angle with
    | 180 -> 180
    | 90 -> -90
    | -90 -> 90
    | _ -> raise (Failure "Angle invalide")
  in
  (face, new_angle)


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



let shuffled_cube = shuffle {unState = etat_final; lastOp=None} 4

let heuristic ps =
  let rec count = function
    | [] -> 0
    | (c, _) :: tail -> (match c with
      | R c | G c | B c | Y c | O c | W c -> if c = 'w' then 1 + count tail else count tail)
  in
  count ps.unState


let rubiks_flag = 1