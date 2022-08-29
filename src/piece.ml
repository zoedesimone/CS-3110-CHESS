exception UnknownPiece of Game_state.piece

(* [type move] represents a possible move [(x;y)]]. *)
type move = int * int

(*[start] is the starting position of the piece as a tuple [(x*y)] *)
type start = int * int

(* [type brd] represents a list of move [(x;y)]]. *)
type brd = move list

(* [type direction] represents possible directions of a piece]. *)
type direction =
  | Vert
  | Horiz
  | DiagQOne
  | DiagQTwo
  | DiagQThree
  | DiagQFour
  | LeftDiag
  | RightDiag
  | LShapeA
  | LShapeB
  | LShapeC
  | LShapeD

(* [type piece_move] represents a possible move]. *)
type piece_move = {
  length : int;
  directions : direction;
}

(* [type moves] represents possible moves of a piece]. *)
type moves = {
  p : Game_state.piece;
  validmove : move list;
}

(** [pos_move_dir] is the list of cells in the positive x direction [xPos], 
negative x direction [xNeg], positive y direction [yPos], negative y direction
[yNeg].*)
type pos_move_dir = {
  xPos : int list;
  xNeg : int list;
  yPos : int list;
  yNeg : int list;
}

(**[range] is the distance from the four sides of the board. *)
type range = {
  xp : int;
  xn: int;
  yp : int;
  yn : int;
}

(** [(--)] is a list from i to j where i > j *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

(**[let_range] is the maximum lengh a piece can move in the x and y
   directions given a start position [start]. Example:
   [let_range (3;4) = {xPos = 5; xNeg = -3; yPos = 4; yNeg = -4}]*)
let range s =
  {
    xp = 8 - (fst s);
    xn = (fst s) -1;
    yp = 8 - (snd s);
    yn = (snd s) -1;
  
  }
  
  (*[qI, qII, qIII, qIIII] are the maximum distance a piece can travel along
  the diagonal in direction of quadrant 1 [qI], quadrant 2 [qII], 
  quadrant 3 [qIII] and quadrant 4 [qIIII].*)
  let qI (s: int * int) = ( -- ) 0 (min (range s).xp (range s).yp )
  let qII (s: int * int) = ( -- ) 0 (min (range s).xn (range s).yp )
  let qIII (s: int * int) = ( -- ) 0 (min (range s).xn (range s).yn)
  let qIIII (s: int * int) = ( -- ) 0 (min (range s).xp (range s).yn)



(**[xpos_list], [xneg_list],[ypos_list],[yneg_list]is the list of all positions 
a piece can move in the positive and negative x and y direction given a start 
position [start]. 
Example: [let make_dir_list (3;4) = {xpos_list = \[1,2,3,4,5\]; 
   xNeg = \[-1,-2\]; yPos = \[1,2,3,4\]; yNeg = \[-1,-2,-3\]}]*)
let xpos_list (start : int * int) = 1 -- (8 - fst start )

let xneg_list (start : int * int) = (1 - fst start ) -- (-1)

let ypos_list (start : int * int) = 1 -- (8 - snd start )

let yneg_list (start : int * int) = (1 - snd start) -- (-1)

(**[move_piece] is the position to which a piece can move given a start
   position on the board [start] , a direction to move [dir] and a
   length to move [len]*)
let move_piece start len dir =
  match dir with
  | Vert -> (fst start, snd start + len)
  | Horiz -> (fst start + len, snd start)
  | DiagQOne -> (fst start + len, snd start + len)
  | DiagQTwo -> (fst start - len, snd start + len)
  | DiagQThree -> (fst start - len, snd start - len)
  | DiagQFour -> (fst start + len, snd start - len)
  | LeftDiag -> (fst start - len, snd start + len)
  | RightDiag -> (fst start + len, snd start + len)
  | LShapeA -> (fst start + (1*len), snd start + (2*len))
  | LShapeB -> (fst start + (1*len), snd start + (-2*len))
  | LShapeC -> (fst start + (2*len), snd start + (1*len))
  | LShapeD -> (fst start + (2*len), snd start + (-1*len))

(**Quadrant I and III are odd, quadrant II and IIII are even.*)

(**Quadrant I has positive x and positive y*)
let min_quadI s = min
  (List.length (xpos_list s)) (List.length (ypos_list s))

(**Quadrant II has negative x and positive y*)
let min_quadII s = min
(List.length (xneg_list s)) (List.length (ypos_list s))

(**Quadrant III has negative x and negative y*)
let min_quadIII s = min
  (List.length (xneg_list s)) (List.length (yneg_list s))

(**Quadrant IIII has positive x and negative y*)
let min_quadIIII s = min
(List.length (xpos_list s)) (List.length (yneg_list s))

(**[vert_moves] is the list of vertical positions to which a piece can
   move starting from position [start] *)
let rec vert_moves start poslist =
  match poslist with
  | [] -> []
  | h :: t -> move_piece start h Vert :: vert_moves start t

(**[horiz_moves] is the list of horizontal positions to which a piece
   can move starting from position [start] *)
let rec horiz_moves start poslist =
  match poslist with
  | [] -> []
  | h :: t -> move_piece start h Horiz :: horiz_moves start t

  (**[move_l_shape s list] is the list of possible positions a Knight can move 
  to, give a starting position [s].*)
let move_lshape (s : int * int) list  = 
  let lista = if ((range s).xp >= 1 && (range s).yp >= 2)  then (move_piece s 1 LShapeA) :: list else list in (**(4,4) to (5,6)*)
  let listb = if ((range s).xn >= 1 && (range s).yn >= 2)  then (move_piece s (-1) LShapeA) :: lista else lista in (**(4,4) to (3,2)*)
  let listc = if (range s).xp >= 1 && (range s).yn >= 2 then move_piece s (1) LShapeB :: listb else listb in (**(4,4) to (5,2)*)
  let listd = if (range s).xn >= 1 && (range s).yp >= 2 then move_piece s (-1) LShapeB :: listc else listc in (**(4,4) to (3,6)*)
  let liste = if (range s).xp >= 2 && (range s).yp >= 1 then move_piece s (1) LShapeC :: listd else listd in (**(4,4) to (6,5)*)
  let listf = if (range s).xn >= 1 && (range s).yn >= 2 then move_piece s (-1) LShapeC :: liste else liste in (**(4,4) to (3,2)*)
  let listg = if (range s).xp >= 2 && (range s).yn >= 1 then move_piece s (1) LShapeD :: listf else listf in (**(4,4) to (6,3)*) 
  let listh = if (range s).xn >= 2 && (range s).yp >= 1 then  move_piece s (-1) LShapeD :: listg else listg  in listh (**(4,4) to (2,5)*)

  (**[move_pawn s list] is the list of possible positions a pawn can move to, 
  given a starting position [s].*)
let move_pawn (s: int * int) list=
(*Direction 1 -UP*)
  let lista  = if (range s).xp >= 1 && ((range s).yp) >= 1 then (move_piece s 1 RightDiag) :: list else list in (*Diagonal Right (4,4) to (5,5)*)
  let listb = if ((range s).xn >= 1 && (range s).yp >= 1) then (move_piece s 1 LeftDiag) :: lista else lista in (*Diagonal Left (4,4) to (3,5)*)
  let listc = if ((range s).yp >= 2) then (move_piece s 2 Vert) :: listb else listb in (*Vertical 2(4,4) to (4,6)*)
   let listd = if ((range s).yp >= 1) then (move_piece s 1 Vert) :: listc else listc in  (*Vertical 1 (4,4) to (4,5)*)
  (*Direction 2 -DOWN*)
  let liste  = if (range s).xn >= 1 && ((range s).yn) >= 1 then (move_piece s (-1) RightDiag) :: listd else listd in (*Diagonal Right (4,4) to (3,3)*)
  let listf = if ((range s).xp >= 1 && (range s).yn >= 1) then (move_piece s (-1) LeftDiag) :: liste else liste in (*Diagonal Left (4,4) to (5,3)*)
  let listg = if ((range s).yn >= 2) then (move_piece s (-2) Vert) :: listf else listf in (*Vertical 2(4,4) to (4,2)*)
  let listh = if ((range s).yn >= 1) then (move_piece s (-1) Vert) :: listg else listg in listh  (*Vertical 1 (4,4) to (4,3)*)

(**[move_king s list] is the list of possible positions a pawn can move to, 
give a starting position [s].*)
   let move_king (s : int*int) list =
    let lista = if (range s).xp>= 1 && (range s).yn >= 1 then (move_piece s 1 DiagQFour) :: list else list in (*DiagQFour*)
   let listb  = if (range s).xn >= 1 && (range s).yn >=1 then (move_piece s 1 DiagQThree) :: lista else lista in 
   let listc  = if (range s).xp >= 1 && ((range s).yp) >= 1 then (move_piece s 1 DiagQOne) :: listb else listb in (*Diagonal Right (4,4) to (5,5)*)
  let listd = if ((range s).xn >= 1 && (range s).yp >= 1) then (move_piece s 1 DiagQTwo) :: listc else listc in (*Diagonal Left (4,4) to (3,5)*)
  let liste = if ((range s).xp >= 1) then (move_piece s 1 Horiz) :: listd else listd in (*Horizontal (4,4) to (5,4)*)
  let listf = if ((range s).xn >= 1) then (move_piece s (-1) Horiz) :: liste else liste in (*Horizontal (4,4) to (5,4)*)
   let listg = if ((range s).yp >= 1) then (move_piece s 1 Vert) :: listf else listf in (*Vertical  (4,4) to (4,5)*)
   let listh = if ((range s).yn >= 1) then (move_piece s (-1) Vert) :: listg else listg in listh (*Vertical  (4,4) to (4,5)*)

 (**[diag_moves start poslist dir] is the list of possible positions a pawn can move to, 
given a list of accessible diagonal positions [poslist].*) 
let rec diag_move start poslist dir=
  match poslist with
  | [] -> []
  | h  :: t ->
      if h > 0 then (move_piece start h dir)  
      :: diag_move start t dir else diag_move start t dir

(**[diag_moves s list] is the list of possible diagonal positions in all four 
quadrant directions qI, qII, qIII, qIV a piece can move to, given a starting 
position [s].*)  
  let diag_moves s list = 
    let lista = if min_quadI s >= 1 then diag_move s (qI s) DiagQOne else list in
    let listb = if min_quadII s >= 1 then diag_move s (qII s)  DiagQTwo @ lista else lista in
    let listc = if min_quadIII s >= 1 then diag_move s (qIII s) DiagQThree @ listb else listb in
    let listd = if min_quadIIII s >= 1 then diag_move s (qIIII s) DiagQFour @ listc else listc in listd


let get_moves p s =
  match p with
  | Game_state.Pawn -> move_pawn s []
  | Game_state.Rook -> (**Moves vertically and horizontally in all lengths*)
    horiz_moves s (xpos_list s)
  @ horiz_moves s (xneg_list s)
  @ vert_moves s (ypos_list s)
  @ vert_moves s (yneg_list s) (*back to foront: yneg- ypos - xneg - xpos*)
  | Game_state.Bishop -> diag_moves s [] (**Moves diagonally in all lengths*) 
  | Game_state.King -> (**Moves vertically, horizontally and diagonally by 1*)
     move_king s []
  | Game_state.Queen -> (**Moves vertically, horizontally, diagoanlly in all lengths*)
      horiz_moves s (xpos_list s)
      @ horiz_moves s (xneg_list s)
      @ vert_moves s (ypos_list s)
      @ vert_moves s (yneg_list s)
      @diag_moves s []
  | Game_state.Knight -> move_lshape s [] (**Moves in an LShape in all directions*)
  | Game_state.Empty -> []



