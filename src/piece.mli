(** [Piece] is a representation of static chess piece data. This module
    represents the possible moves by each chess piece. *)

exception UnknownPiece of Game_state.piece
(** Raised when an unknown piece is encountered. *)

type move = int * int
(** The type of a move of a chess piece. *)

type brd
(** The type of the board. *)

type start = int * int
(** The type of the starting position of the board. *)

val get_moves : Game_state.piece -> start -> move list
(** [get_moves] is the list of possible moves of piece[p] at starting
    position [start] on the board [brd]. Raises: [UnknownPiece piece] if
    piece [p] is not a valid piece name in [p]. *)
