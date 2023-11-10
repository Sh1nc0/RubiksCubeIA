
type face = UP of string | DOWN of string | LEFT of string | RIGHT of string | FRONT of string | BACK of string

(* red, green, blue, yellow, orange, white *)
type couleur = R of char | G of char | B of char | Y of char | O of char | W of char

type piece = (couleur * int)

let etat_final : piece list = [
  (W 'w', 1); (W 'w', 2); (Y 'w', 3); (W 'w', 4); (*0, 1, 2, 3*)
  (R 'r', 1); (R 'r', 2); (R 'r', 3); (R 'r', 4); (*4, 5, 6, 7*)
  (B 'b', 1); (B 'b', 2); (B 'b', 3); (B 'b', 4); (*8, 9, 10, 11*)
  (G 'g', 1); (G 'g', 2); (G 'g', 3); (G 'g', 4); (*12, 13, 14, 15*)
  (Y 'y', 1); (Y 'y', 2); (Y 'y', 3); (Y 'y', 4); (*16, 17, 18, 19*)
  (O 'o', 1); (O 'o', 2); (O 'o', 3); (O 'o', 4); (*20, 21, 22, 23*)
]
let rec cat = function
  ([], y) -> y
  | (x::xs, y) -> x :: cat (xs, y)

let rec skip = function
  ([], _) -> []
  | (_::xs as xs1, c) -> if c > 0 then skip(xs, c - 1) else xs1

let rec take = function
  ([], _) -> []
  | (x::xs, c) -> if c > 0 then x :: take(xs, c - 1) else []

let rotate_right l i =
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

let indice_rotation x = match x with
  | UP pid -> [0; 1; 2; 3; 7; 6; 8; 11; 13; 12; 18; 17]
  | DOWN pid -> [21; 20; 23; 22; 4; 5; 9; 10; 14; 15; 19; 16]
  | RIGHT pid -> [11; 8; 9; 10; 2; 1; 6; 5; 20; 23; 14; 13]
  | LEFT pid -> [17; 18; 19; 16; 0; 3; 12; 15; 22; 21; 4; 7]
  | FRONT pid -> [12; 13; 14; 15; 3; 2; 11; 10; 23; 22; 19; 18]
  | BACK pid -> [6; 7; 4; 5; 1; 0; 17; 16; 21; 20; 9; 8]

let rec rotation angle indices =
  match angle with
  | 90 ->
    let (r, l) = split indices 4 in
    let rotate_r = rotate_right r 1 in
    let rotate_l = rotate_right l 2 in
    let j = join rotate_r rotate_l in
    j

  | -90 -> rotation 90 (rotation 90 (rotation 90 indices))
  | 180 -> rotation 90 (rotation 90 indices)
  | _ -> raise (Failure "Angle invalide")

let rotate angle face =
  let indices = indice_rotation face in
  let new_indices = rotation angle indices in
  let rec aux l i =
    match l with
    | [] -> []
    | (x, _) :: r -> if List.mem i new_indices then (x, i) :: aux r (i + 1) else aux r (i + 1)
  in aux etat_final 0

let extraire_caracteres (liste_pieces : piece list) : char list =
    List.map (fun (couleur, _) ->
      match couleur with
      | R c | G c | B c | Y c | W c | O c -> c
    ) liste_pieces

let afficher_liste_caracteres (liste_caracteres : char list) : unit =
      List.iter (fun c -> Printf.printf "%c" c) liste_caracteres;
      print_endline ""

let () =
  let r = rotation 180 [1;2;3;4;5;6;7;8;9;10;11;12] in
  List.iter (Printf.printf "%d ") r;