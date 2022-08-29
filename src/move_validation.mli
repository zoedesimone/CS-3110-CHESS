(** [Move_validation] handles ensuring the correctness of the board and
    state while making a move.*)

val attempt_move : Game_state.board ->
  Game_state.board_coord ->
  Game_state.board_coord ->
  Game_state.board
  * Game_state.result
  * (Game_state.piece * Game_state.color)
(**[attempt_move board board_coord board_coord] validates all board
   considerations (checks, blocked pieces, castling ect.) Returns
   considerations (checks, blocked pieces, castling ect.) Returns
   [board, Legal] if the move is allowed*)
