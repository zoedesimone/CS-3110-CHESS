(** [game_state] holds information about the board and the current state
    of the game, as well as methods to generate new states with given
    modifications. *)

(** [piece] represents possible chess pieces. *)
type piece =
  | Pawn
  | Rook
  | Bishop
  | King
  | Queen
  | Knight
  | Empty

type castle_rights = {
  king_side : bool;
  queen_side : bool;
}
(** [castle_rights] represents a colors ability to castle to either side
    of the board.*)

type time = int * int
(** [time] represents the current time of the game's chess clock. White
    first, then black. *)

(** [color] represents the color of a piece on a given square. *)
type color =
  | White
  | Black
  | NoPiece

val get_piece_str : piece * color -> string
(** [get_piece_str] Returns the string representation of a given piece.
    Example: (Pawn, White) -> "♟︎"*)

type board
(** [board] is the abstract representation of a chess board. *)

type board_coord = {
  rank : int;
  file : int;
}
(** [board_coord] represents a location on a chess board, using integers
    to represent ranks and mapping files A-H to 1-8 *)

type game_state = {
  board : board;
  white_taken : piece list;
  black_taken : piece list;
  time : time;
}
(** [game_state] stores a board and information about the game,
    including the what pieces have been taken and the current time on
    the chess clock *)

(** [result] is a flag to tell users if a board is legal or not*)
type result =
  | Legal
  | Illegal

(**[get_king] is the board voordinate with the king of the inputted color *)
val get_king : board -> color -> board_coord

val get_time : game_state -> time
(** [get_time] is the current time remaining for both players*)

val set_time : game_state -> time -> game_state
(** [set_time] is an updated game_state with a give time on the clock*)

val set_square : board -> board_coord -> piece * color -> board
(** [set_square] is the board with the given square changed to the given
    piece*)

val get_square : board -> board_coord -> piece * color
(** [get_square] is the piece on the board at the given coordinate *)

val move_piece : board -> board_coord -> board_coord -> board
(** [move_piece] returns a board representing the result of a move *)

val swap_turn : board -> board
(** [swap_turn] returns a board with the opposite player to move *)

val get_castle_availability : board -> color -> castle_rights
(** [get_castle_availability] returns a boolean representing if a color
    is still able to castle*)

val set_castle_availability : board -> color -> castle_rights -> board
(** [set_castle_availability] returns a board with*)

val get_en_passant_target : board -> board_coord option
(**[get_en_passant_target is Some board coodinate that can be attacked by en 
   passant or None] *)

val set_en_passant_target : board -> board_coord option -> board
(**[set_en_passant_target] returns a board with an updated en passant target 
   option*)

val get_board_from_FEN : string -> board
(** [get_board_from_FEN] returns a board representing a FEN string. *)

val color_to_move : board -> color
(** [color_to_move] returns the color of the current player. *)

val board_to_list : board -> string list
(** [board_to_list] is the board in a a string list representation,
    using the standard letter convention for the pieces*)

val compare_game_board : board -> board -> bool
(** [compare_game_board] is true if board_to_list generates the same
    list. Only checks for piece location, doesn't check things like
    castle availability or en passant target.*)
