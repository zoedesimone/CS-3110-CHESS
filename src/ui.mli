(** [Ui] handles displaying the board graphics and various screens in
    the terminal *)

val update_display : Game_state.game_state -> unit
(** [update_display] updates the display to show a new board *)

val show_start : unit
(** [show_start] displays the title screen of the game *)

val show_end : bool -> unit
(** [show_end] displays the closing screen of the game if game is over *)

val print_color : int -> int -> string -> unit
(** [print_color] adds the relevant ANSI strings to display text with
    the given color and background.*)

val instructions : unit -> unit
(** [instructions] prints a how to play screen*)