let etat_initial = "a"

type face = UP of string | DOWN of string | LEFT of string | RIGHT of string | FRONT of string | BACK of string

(* red, green, blue, yellow, orange, white *)
type couleur = R of char | G of char | B of char | Y of char | O of char | W of char

type piece = (couleur * int)

let etat_final : piece list = [
  (R 'r', 1); (R 'r', 2); (R 'r', 3); (R 'r', 4);
  (Y 'y', 1); (Y 'y', 2); (Y 'y', 3); (Y 'y', 4);
  (W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4);
  (O 'o', 1); (O 'o', 2); (O 'o', 3); (O 'o', 4);
  (G 'g', 1); (G 'g', 2); (G 'g', 3); (G 'g', 4);
  (B 'b', 1); (B 'b', 2); (B 'b', 3); (B 'b', 4)
]
let indice_rotation x = match x with
  | UP pid -> [8; 9; 11; 10; 2; 3; 12; 14; 21; 20; 7; 5]
  | DOWN pid -> [17; 16; 18; 19; 0; 1; 13; 15; 23; 22; 6; 4]
  | RIGHT pid -> [14; 12; 13; 15; 11; 9; 3; 1; 16; 18; 23; 21]
  | LEFT pid -> [5; 7; 6; 4; 8; 10; 20; 22; 19; 17; 0; 2]
  | FRONT pid -> [20; 21; 23; 22; 10; 11; 14; 15; 18; 19; 6; 7]
  | BACK pid -> [3; 2; 0; 1; 9; 8; 5; 4; 17; 16; 13; 12]

let rotation angle face = "dsqds"