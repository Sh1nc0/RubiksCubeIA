(** The Klotski Puzzle *)


(**

{b Copyright  :}  (c) September 3rd, 2023, José Martinez, École polytechnique de l'université de Nantes

{b License    :}  AllRightsReserved

{b Maintainer :} Julien Cohen for the OCaml version
{b Stability  :}  Experimental, Linux
{b Portability:}  OCaml 4.14

{b Technical Aspect.}

This problem has been used to determine the benefit of using hashed maps.

As illustrated in the following Table for the 'aBFS' function under the uncompiled ghci environment, the performances are not that good.
Operations on the map itself are logarithmic.
However, comparing states that are ordered sets of pieces is costly, even if linear.
All in all the complexity of inserting or retrieving /all/ the states in the graph is in $k.O(p.n.lg n)$ where $n$ is the number of nodes, $p = 10$ is the constant number of pieces in this situation, and $k$, a constant associated to comparing pieces and sets.
Notice that there are duplicates, which means that the number of retrieval is larger than the number of insertions, the one reported on the Table in the column Size.
However, event with ten millions of nodes, the logarithm is "only" around 23.

@
| Depth | Size      | Time (s) | Space (bytes)   |
| ----- | --------- | -------- | --------------- |
| 10    |       966 |     0.15 |      74,365,600 |
| 20    |     8,045 |     1.26 |     848,023,152 |
| 30    |    19,604 |     3.46 |   2,182,932,568 |
| 40    |    39,086 |     6.78 |   4,412,161,544 |
| 50    |   146,328 |    25.26 |  16,480,058,008 |
| 60    |   532,710 |   100.68 |  64,335,107,488 |
| 70    | 1,486,669 |   307.06 | 190,574,446,888 |
| 80    | 3,184,118 |   701.22 | 428,834,449,168 |
| 90    | 5,329,079 | 1,316.16 | 747,697,360,240 |
@

With an hashed map, operations are expected to be in constant time on the map.
Furthermore, hashing is in $O(p)$ but done only once, when hashing the node to insert or delete.
All in all, the complexity of inserting or retrieving a state in the graph is in $k'.O(p.n)$ where $k'$ is some other constant related to hashing and comparing hashes.
The constant $k'$ is expected to be larger than $k$ for various reasons.
In contrast, we avoid the logarithmic cost.
This improvement is to be seen in the following Table by comparing it to the previous experiments.

@
| Depth | Size      | Time ordered (s) | Space (bytes)   |
| ----- | --------- | ---------------- | --------------- |
| 10    |       966 |             0.08 |      71,558,472 |
| 20    |     8,045 |             0.78 |     736,622,920 |
| 30    |    19,604 |             1.93 |   1,831,397,816 |
| 40    |    39,086 |             3.89 |   3,620,530,832 |
| 50    |   146,328 |            13.76 |  12,749,229,592 |
| 60    |   532,710 |            51.91 |  47,334,444,608 |
| 70    | 1,486,669 |           151.33 | 135,384,097,056 |
| 80    | 3,184,118 |           336.27 | 296,698,115,408 |
| 90    | 5,329,079 |           570.70 | 509,379,611,768 |
@

In practice, the advantage is a constant factor around... only 2 as computed in the Table below!
If the ratio of $k.O(p.n.lg n) / k'.O(p.n) \simeq 2$, it means that we roughly have $k.lg n / k' \simeq 2$.
Then, with $\lg n = 23$, it comes that $23.k / k' \simeq 2 \iff k' / k \simeq 46$.
In other words, processing the hashing and the hashed data structure appears extremely costly!

@
| Depth | Time ordered (s) | Time hashed (s) | Speed-up |
| ----- | ---------------- | --------------- | -------- |
| 10    |             0.15 |            0.08 |    1.875 |
| 20    |             1.26 |            0.78 |    1.615 |
| 30    |             3.46 |            1.93 |    1.793 |
| 40    |             6.78 |            3.89 |    1.743 |
| 50    |            25.26 |           13.76 |    1.835 |
| 60    |           100.68 |           51.91 |    1.940 |
| 70    |           307.06 |          151.33 |    2.029 |
| 80    |           701.22 |          336.27 |    2.085 |
| 90    |         1,316.16 |          570.70 |    2.306 |
@

When compiling the two variants of the code, we obtain the following Table.
The speed-up from the ordered version to the hashed one remains around 2.

@
| Depth | Time ordered (s) | Time hashed (s) | Speed-up | Compilation speed-up (ordered) | Compilation speed-up (hashed) |
| ----- | ---------------- | --------------- | -------- | ------------------------------ | ----------------------------- |
| 10    |            0.377 |           0.392 |    0.962 |                          0.212 |                         0.204 |
| 20    |            0.664 |           0.519 |    1.280 |                          1.175 |                         1.503 |
| 30    |            1.112 |           0.680 |    1.635 |                          1.736 |                         2.838 |
| 40    |            1.679 |           1.119 |    1.500 |                          2.317 |                         3.476 |
| 50    |            5.573 |           2.756 |    2.022 |                          2.470 |                         4.993 |
| 60    |           21.606 |          10.039 |    2.152 |                          5.171 |                         5.171 |
| 70    |           65.244 |          29.649 |    2.200 |                          5.104 |                         5.104 |
| 80    |          132.716 |          63.654 |    2.085 |                          2.535 |                         5.283 |
| 90    |          243.141 |         112.066 |    2.170 |                          2.347 |                         5.093 |
@

The compilation improvement is not visible for the first levels due to various work around running the process itself.
However, on larger figures, we see an improvement.
But there is a surprising result too.
The compilation speed-up varies largely between 2 and 5 for the ordered map version, whereas it converges to 5 for the hashed map case.
(Both cases are much less than the common factor of 10...)
This is probably due to the memory management, though we are only adding new nodes in the map hence avoiding much of the burden of the garbage collector.

 *)


(** {2 Representation of the problem} *)
(** {5 Pieces} *)



(** Although this information is not absolutely required, it is useful for visualising the game.*)
type identifier = char

(** A piece has a width and height and is located somewhere on the board as defined by a Cartesian coordinate.
 We orient it from the left to the right, and from top to bottom.

 In addition, each piece is identified by a character, which is used in order to draw the board.
 In fact, this additional piece (!) of information introduces a problem that is to be solved by the 'Eq' and 'Ord' instances.
 It happens, that on a board the identifier is /equivalent/, not equal, to the rest of the attributes, i.e., the coordinates /and/ dimensions.
 *)
type piece = 
   { pid    : identifier
   ; x      : int
   ; y      : int
   ; width  : int
   ; height : int
   }

let piece a b c d e = 
  { pid = a ; x = b ; y = c ; width = d ; height = e } 

(**
 Since we have an identifier, it is _almost_ sufficient to compare pieces for equality.
 We have to take into account that they are present in different states.
 So, even if the identifier is enough to differentiate them in any single state, we still need to take into account the coordinates in order to differentiate them in various states.
 In contrast, the width and height are constants over the whole graph, hence excluded from the comparison.

 However, it turns out that the internal comparison that can be build on a compact tuple is much faster than trying to decompose it into three attributes among five.
 This does not pay back enough...
 
instance Eq Piece where
   p == q = 
      pid p == pid q &&
      x p == x q &&
      y p == y q
 *)

(**
  As for equality, thanks to the identifier, it would sufficient to compare pieces for ordering, plus the coordinates in a state world.
 This is almost as efficient as the native code, but the latter remains the best nevertheless. 

instance Ord Piece where
   p <= q =
      pid p < pid q || (pid p == pid q && (x p < x q || (x p == x q && y p <= y q)))

 *)

let string_of_piece {pid; x; y; width; height} = 
  "Rect(" ^ (Char.escaped pid) 
    ^ ",(" ^ string_of_int x
    ^ "," ^ string_of_int y
    ^ "),(" ^ string_of_int (x + width - 1)
    ^ "," ^ string_of_int (y + height - 1)
    ^ "))"

(**
  The set of coordinates of the board for the abscissas.
 *)
let xs = List.init 4 (fun x -> x) (*[0..3]*)

(**  The set of coordinates of the board for the ordinates.
 *)
let ys = List.init 5 (fun x -> x) (*[0..4]*)


(** {5 States} *)

type 'a set = 'a BatSet.t 
(** We use polymorphic sets, i.e. we use polymorphic comparisons = and <, which is assumed for Klotski *) 

(**
  The state identifies itself with the set of pieces on the board.
*)
type k_state = { unState : piece set } 

let string_of_state { unState } =
  BatSet.fold (fun e r -> string_of_piece e ^ ", " ^ r)
    unState
    ""



(**
  The equivalence class representative of a state is nothing more than its coordinates and dimensions.

 For instance, at a depth of 30, there are 118,756 states with a mere equality, which increases to 704,720 at a depth of 50.
 In comparison, with this "true" equality, there are respectively 3,159 and 4,989.
 Despite the cost of computing the state representative, it pays back largely on processing time, e.g., from 40 s down to 2 s for depth 50.*)
let equivalentState s : (int*int*int*int) set = BatSet.map (fun p -> (p.x, p.y, p.width, p.height)) s.unState

(**
  This equality instance provides a very important heuristic.
 States are not simply equal by containing the same pieces at the same place.
 Since we can have pieces of the same dimensions, and that they are not marked (except by the color required to colour them on a visualisation), we have to take into account these equivalences.
 
 For instance, in the classical game, since we have four unit squares and four vertical bars, this reduces the search $4!.4! = 576$ times, which is hardly negligible.

 In fact, without considering the colour of the piece on the first place, this would not have been necessary, the default equality would have been enough!
*)
let equal_state s1 s2 =
  BatSet.equal (equivalentState s1) (equivalentState s2)



(**
  For the same reason than for equality, we have to restrict the ordering by removing the so-called identifier.
*)
let compare ps qs =
  BatSet.compare (equivalentState ps) (equivalentState qs)

(*
(**
  A first hashing class instance to define in order to be able to use 'Data.HashMap'.
*) 
instance Hashable Piece where
  hashWithSalt salt (Piece _ x y w h) =
    salt `hashWithSalt` x
         `hashWithSalt` y
         `hashWithSalt` w
         `hashWithSalt` h

(**
  A second hashing class instance to define in order to be able to use 'Data.HashMap'.
*)
instance Hashable State where
  hashWithSalt salt (State ps) = hashWithSalt salt ps
 *)


(** {2 Some problems to be solved} *) 

(** {5 Trivial board for testing} *)

(**
  Another starting position to enable some simple tests.
*)
let anotherKlotski =
  { unState =
      BatSet.of_list
        [ piece '!' 0 0 1 2 ; piece '#' 1 0 2 2 
        ]
  }



(** {5 Standard Klotski} *)

(**
  This is only one of the possible starting positions for the game.
 It is the classical one.
*)
let aKlotski =
  { unState =
      BatSet.of_list
        [ piece '!' 0 0 1 2 ; piece '#' 1 0 2 2 ; piece '|' 3 0 1 2
        ; piece 'I' 0 2 1 2 ; piece '=' 1 2 2 1 ; piece ':' 3 2 1 2 
        ; piece 'o' 1 3 1 1 ;                     piece '+' 2 3 1 1 
        ; piece '*' 0 4 1 1 ;                     piece 'x' 3 4 1 1
        ]
  }


(**
  This is the place where the main piece has to be located in order to go through the exit gate.
*)
let theFinalPosition = piece '#' 1 3 2 2

(*
(**
  The dimensions of the pieces have been directly associated to them.
 However, it is possible to declare them as constant data since they must never be modified by the sliding operations.
 This seems the right way to do it.
 However, using them "redundantly" in the 'Piece' data type allows for faster operations on the equality and ordering of the pieces and the states.

 In fact, we use this function only for testing the pre-condition, hence ensuring that they operations do not modify the dimensions of any piece.
*)
dimensions : M.HashMap Identifier (int, int)
dimensions =
   M.fromList
      [ ('!', (1, 2)), ('#', (2, 2)), ('|', (1, 2))
      , ('I', (1, 2)), ('=', (2, 1)), (':', (1, 2))
      , ('o', (1, 1)),                ('+', (1, 1))
      , ('*', (1, 1)),                ('x', (1, 1))
      ]

(**
  To each piece is associated a colour.
 This is to be used for showing the board, especially on the DOT GraphViz output.
*)
colours : M.HashMap Identifier string
colours =
   M.fromList
      [ ('!', "blue") , ('#', "red") , ('|', "cyan")
      , ('I', "green"), ('=', "gray"), (':', "olive")
      , ('o', "pink1"),                ('+', "pink2")
      , ('*', "magenta"),              ('x', "violet")
      ]
 *)

(** {5 Pennant variant} *)

(** The Pennant variant is much simpler to solve than the standard position.*)

let pennantKlotski = 
  { unState =
      BatSet.of_list
        [ piece '#' 0 0 2 2; piece '|' 2 0 1 2; piece '!' 3 0 1 2
          ; piece '=' 0 2 2 1
          ; piece '~' 0 3 2 1; piece 'o' 2 3 1 1; piece '+' 3 3 1 1 
          ; piece '-' 0 4 2 1; piece '*' 2 4 1 1; piece 'x' 3 4 1 1
        ]
  }
      
let pennantFinalPosition = piece '#' 2 0 2 2



let dimensions =
  [ ('#', (2, 2)); ('|', (1, 2)); ('!', (1, 2))
      ; ('=', (2, 1))
      ; ('~', (2, 1)); ('o', (1, 1)); ('+', (1, 1))
      ; ('-', (2, 1)); ('*', (1, 1)); ('x', (1, 1))
      ]


let colours =
   
  [ ('#', "red")   ; ('|', "cyan")   ; ('!', "blue")
    ; ('=', "gray")
    ; ('~', "yellow"); ('o', "pink1")  ; ('+', "pink2")
    ; ('-', "orange"); ('*', "magenta"); ('x', "violet")
  ]
 

(** {2 Describing transitions } *)

(**   The puzzle is solved when the main piece final position belongs to the state description.
*)
let isSolved {unState} = BatSet.mem theFinalPosition unState


(**   The parameterised operations are in the number of four, each one consisting in moving a piece in one of the cardinal directions by one position.
*)
type move = 
   Up of identifier
 | Down of identifier
 | Left of identifier
 | Right of identifier
             
let string_of_move = function
  | Up pid -> Char.escaped pid ^ " ↑"
  | Down pid -> Char.escaped pid ^ " ↓"
  | Left  pid -> Char.escaped pid ^ " ←"
  | Right pid -> Char.escaped pid ^ " →"

(**
  Sub-set of the coordinates that are occupied by a piece on the board.
*)
let pieceCoordinates { x ; y ; width ; height ; _ } : (int * int) set =
  Miscellaneous.cartesianProduct [x; x + width - 1] [y; y + height - 1]
  (* Rem : la largeur et la hauteur valent toujours 1 ou 2 dans notre exemple *)

let string_of_coordinate = Pairs.string_of_pair string_of_int string_of_int

(**
  Sub-set of the coordinates that are not occupied by any piece on the board.
*)
let freeCoordinates (ps:k_state) : (int * int) set=
  let c = BatSet.map pieceCoordinates ps.unState
  in let c_flat = BatSet.fold BatSet.union c BatSet.empty
       and p = Miscellaneous.cartesianProduct xs ys
     in BatSet.diff p c_flat


(** {2 Building a STATEPROBLEM module} *)

module KlotskiProblem = 
  (StateProblem.StateProblem 
     (struct 
       type state = k_state 
       let string_of_state = string_of_state 
     end)
     (struct 
       type op = move 
       let string_of_op = string_of_move 
     end)
     (Additive.IntC)
   )


(** {2 Legal transitions} *)


(**
  The transitions consist of four kinds of operations described by 'Move'.
 
 The cost is always one.
@param s, a state 
@return the transitions that can be expanded from s with the application of a single operation *)
let moves s =
  begin

    assert (BatSet.for_all (fun p -> List.mem_assoc p.pid colours) s.unState) ;
    assert (BatSet.for_all (fun p -> List.mem_assoc p.pid dimensions) s.unState) ;

    assert (BatSet.for_all (fun p -> List.assoc p.pid dimensions = (p.width,p.height)) s.unState) ;
 

(*
   assert ([ p
                | p@(Piece _ x y _ _) <- S.toList ps
                , any ((`notElem` xs) . fst) (pieceCoordinates p)
                , any ((`notElem` ys) . snd) (pieceCoordinates p)
                ] = []) ;*)
  let fs = freeCoordinates s 
  in let free p = BatSet.mem p fs
     in
  let pslist = BatSet.elements s.unState in 

  let up =
    let psl' = 
      List.filter 
        (fun p -> free (p.x,p.y-1) && free (p.x + p.width -1 ,p.y-1))
        pslist
    in List.map 
         (fun p ->
           { KlotskiProblem.startState = s ;
             operation = Up p.pid ;
             endState = {unState = BatSet.add {p with y = p.y-1} (BatSet.remove p s.unState)} ;
             cost = 1})
         psl'

  and down =
    let psl' = 
      List.filter 
        (fun p -> free (p.x,p.y+p.height) && free (p.x + p.width -1 ,p.y+p.height))
                 pslist
    in List.map 
         (fun p ->
           { KlotskiProblem.startState = s ;
             operation = Down p.pid ;
             endState = {unState = BatSet.add {p with y = p.y+1} (BatSet.remove p s.unState)} ;
             cost = 1})
         psl'

  and left =
    let psl' =
      List.filter 
        (fun p -> free (p.x-1,p.y) && free (p.x-1 ,p.y+p.height-1) )
        pslist
    in List.map 
         (fun p ->
           { KlotskiProblem.startState = s ;
             operation = Left p.pid ;
             endState = {unState = BatSet.add {p with x = p.x-1} (BatSet.remove p s.unState)} ;
             cost = 1})
         psl'

  and right =
    let psl' = 
      List.filter 
        (fun p -> free (p.x+p.width,p.y) && free (p.x+p.width ,p.y+p.height-1))
        pslist
    in List.map 
         (fun p ->
           { KlotskiProblem.startState = s ;
             operation = Right p.pid ;
             endState = {unState = BatSet.add {p with x = p.x+1} (BatSet.remove p s.unState)} ;
             cost = 1})
         psl'

   in (up @ down @ left @ right)
  end
  

(** {2 Other tools} *)

(**
  This simple __heuristic__ computes how many tiles are not at the right place.
 It is a very crude approximation of the actual cost.
 It is as if we could make tiles fly over the board!

@param ps a state
@return an evaluation of a strictly minimal cost from this state to a solution *)
let manhattanHeuristic ps = 
  let p = BatSet.find_first (fun p'-> p'.pid = '#') ps.unState
  in
  abs (p.x - theFinalPosition.x) + abs (p.y - theFinalPosition.y)


(*
(**
  A solution to a scrambled board
*)
solverBFS
   : int                       (* dMax, the maximal search depth *)
   -> State                     (* a puzzle board *)
   -> [Solution Move State int] (* the corresponding solutions, should one exist *)
solverBFS dMax = BFS.solver dMax moves isSolved

(**
  A solution to a scrambled board
*)
solverAstar
   : int                       (* dMax, the maximal search depth *)
   -> State                     (* a puzzle board *)
   -> [Solution Move State int] (* the corresponding solutions, should one exist *)
solverAstar dMax = Astar.solver dMax moves isSolved manhattanHeuristic dMax

solverAstarExploredGraph dMax = 
   Astar.solverExploredGraph dMax 
                             moves
                             isSolved
                             (\ s _ -> manhattanHeuristic s)
                             dMax



klotskiBoardIdentifier : State -> string
klotskiBoardIdentifier (State ps) =
   concat
      [ c : show x ++ '.' : show y
      | Piece c x y w h <- S.toList ps
      ]

klotskiBoardRepresentation : State -> string
klotskiBoardRepresentation (State ps) =
   unwords
      [ "<<table border = \"1\" cellborder = \"0\" cellpadding = \"0\" cellspacing = \"0\">"
      , unwords
           [ "<tr>" ++
             unwords
                [ "<td bgcolor=\"" ++ fromMaybe "white" (M.lookup c colours) ++ "\">" ++ [c] ++ "</td>"
                | x <- xs
                ,  let c = fromMaybe ' ' (M.lookup (x, y) pos)
                ] ++
            "</tr>"
           | y <- ys
           ]
      , "</table>>"
      ]
      where
         pos = 
            M.fromList
               [ ((x, y), c)
               | p@(Piece c _ _ _ _) <- S.toList ps
               , (x, y) <- S.toList (pieceCoordinates p)
               ]

explore d = solverAstarExploredGraph d aKlotski
 *)





let klotski_flag = 1
