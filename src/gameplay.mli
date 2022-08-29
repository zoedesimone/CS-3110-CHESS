(** [Gameplay] is a collection of methods to handle interaction of the
    game with its surroundings (player / computer). This includes taking
    and validating moves and incrementing the clock.*)

type valid = {
  start : Game_state.board_coord;
  next : Game_state.board_coord;
}
(** [valid] represents a possible chess piece move *)

type move =
  | Valid of valid
  | End
      (** [move] represents whether player's input is to End game or
          possible move *)

val check : string -> valid
(** Checks if inputted move is valid (square on board) and returns move
    if valid and raises Failure if invalid move*)

val take_move : string -> move
(** Asks for user's input and checks the user's input, returns user's
    input as type move if valid input but if invalid asks again for new
    input until correct*)

val print_time :
  Game_state.color -> Game_state.game_state -> int -> Game_state.time
(** Returns new time of both players based on how long it took for one
    player to move*)

val check_end : move -> bool
(** Returns true if player asks to quit the game and false if inputted a
    move*)

val return_start : move -> bool -> Game_state.board_coord
(** Returns start and end board square of the inputted move*)

val still_time : int * int -> bool
(** Returns true if both players have remaining time left and false if
    either player has 0 seconds left*)
