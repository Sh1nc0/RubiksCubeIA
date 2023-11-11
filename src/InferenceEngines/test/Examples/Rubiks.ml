(* ###### MARK: TYPES ######*)
(* red, green, blue, yellow, orange, white *)
type couleur = R of char | G of char | B of char | Y of char | O of char | W of char

type piece = (couleur * int)

type face =  UP | DOWN | LEFT | RIGHT | FRONT | BACK

(* ###### MARK: FONCTIONS UTILES ######*)

let rec cat = function
  ([], y) -> y
  | (x::xs, y) -> x :: cat (xs, y)

let rec skip = function
  ([], _) -> []
  | (_::xs as xs1, c) -> if c > 0 then skip(xs, c - 1) else xs1

let rec take = function
  ([], _) -> []
  | (x::xs, c) -> if c > 0 then x :: take(xs, c - 1) else []

let cycle_right l i =
  let len = List.length l in
  let i = (len - (i mod len)) mod len in
  cat (skip (l, i), take (l, i))

let rec split lst n =
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

let etat_final : piece list = [
  (W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4); (*0, 1, 2, 3*)
  (R 'r', 1); (R 'r', 2); (R 'r', 3); (R 'r', 4); (*4, 5, 6, 7*)
  (B 'b', 1); (B 'b', 2); (B 'b', 3); (B 'b', 4); (*8, 9, 10, 11*)
  (G 'g', 1); (G 'g', 2); (G 'g', 3); (G 'g', 4); (*12, 13, 14, 15*)
  (Y 'y', 1); (Y 'y', 2); (Y 'y', 3); (Y 'y', 4); (*16, 17, 18, 19*)
  (O 'o', 1); (O 'o', 2); (O 'o', 3); (O 'o', 4); (*20, 21, 22, 23*)
]

let indice_rotation x = match x with
  | UP -> [0; 1; 2; 3; 7; 6; 8; 11; 13; 12; 18; 17]
  | DOWN -> [21; 20; 23; 22; 4; 5; 9; 10; 14; 15; 19; 16]
  | RIGHT -> [11; 8; 9; 10; 2; 1; 6; 5; 20; 23; 14; 13]
  | LEFT -> [17; 18; 19; 16; 0; 3; 12; 15; 22; 21; 4; 7]
  | FRONT -> [12; 13; 14; 15; 3; 2; 11; 10; 23; 22; 19; 18]
  | BACK -> [6; 7; 4; 5; 1; 0; 17; 16; 21; 20; 9; 8]

let rec extraire_pieces indices s =
  match indices with
  | [] -> []
  | i :: tail -> (List.nth s i) :: extraire_pieces tail s

let rec rotation_angle angle s =
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

let rotation face angle s =
  let indices = indice_rotation face in
  let pieces = extraire_pieces indices s in
  let rotated_pieces = rotation_angle angle pieces in
  let rec remplacer_pieces indices pieces s =
    match indices, pieces with
    | [], [] -> s
    | i :: tail, p :: tail2 -> remplacer_pieces tail tail2 (List.mapi (fun index (c, _) -> if index = i then p else (c, index + 1)) s)
    | _ -> raise (Failure "Erreur de rotation")
  in
  remplacer_pieces indices rotated_pieces s


let extraire_caracteres (liste_pieces : piece list) : char list =
    List.map (fun (couleur, _) ->
      match couleur with
      | R c | G c | B c | Y c | W c | O c -> c
    ) liste_pieces

let afficher_liste_caracteres liste =
  List.iter (fun c -> Printf.printf "%c " c) liste;
  print_newline ()

let pretty_print cube =
  let chars = extraire_caracteres cube in
  let i index = (List.nth chars index) in
  Printf.printf "    %c %c\n" (i 4) (i 5);
  Printf.printf "    %c %c\n" (i 7) (i 6) ;
  Printf.printf "%c %c %c %c %c %c %c %c\n" (i 16) (i 17) (i 0) (i 1) (i 8) (i 9) (i 20) (i 21);
  Printf.printf "%c %c %c %c %c %c %c %c\n" (i 19) (i 18) (i 3) (i 2) (i 11) (i 10) (i 23) (i 22);
  Printf.printf "    %c %c\n" (i 12) (i 13);
  Printf.printf "    %c %c\n" (i 15) (i 14);
  print_newline ()


let () =
  pretty_print etat_final;
  let rotated_cube  = rotation RIGHT 90 etat_final in
  pretty_print rotated_cube;

