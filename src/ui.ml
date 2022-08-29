(*https://github.com/ix/ocaml-termcolors/blob/master/termcolor.ml*)
let foreground col text =
  let open Printf in
  sprintf "\x1B[38;5;%dm%s\x1B[0m" col text

let background col text =
  let open Printf in
  sprintf "\x1B[48;5;%dm%s\x1B[0m" col text

let color_string front back text =
  let b = background back text in
  foreground front b

let print_color front back text =
  let open Printf in
  print_string (color_string front back text)

let square_size = (7, 3)

let light_color = 221

let dark_color = 94

let black_code = 16

let white_code = 255

let time_to_string ((w, b) : Game_state.time) (color : Game_state.color)
    =
  match color with
  | White -> string_of_int w
  | Black -> string_of_int b
  | NoPiece -> failwith "Cannot get time of unknown color."

let reverse lst = List.fold_left (fun lrev b -> b :: lrev) [] lst

let get_taken_string (state : Game_state.game_state) color =
  let pieces =
    match color with
    | Game_state.White -> state.white_taken
    | Game_state.Black -> state.black_taken
    | Game_state.NoPiece ->
        failwith "Cannot get taken pieces from NoPiece"
  in
  let a = List.map (fun a -> (a, color)) pieces in
  let b = List.map Game_state.get_piece_str a in
  List.fold_left ( ^ ) "" b

let get_sidebar_row state row =
  let sidebar =
    "\n    Black time remaining:\n    "
    ^ time_to_string (Game_state.get_time state) Game_state.Black
    ^ "\n\n     "
    ^ get_taken_string state Game_state.White
    ^ "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n     "
    ^ get_taken_string state Game_state.Black
    ^ "\n\n    White time remaining:\n    "
    ^ time_to_string (Game_state.get_time state) Game_state.White
    ^ "\n\n    "
  in
  let split = reverse (String.split_on_char '\n' sidebar) in
  try List.nth split row with _ -> ""

let rec string_of_length symb length =
  match length with
  | x when x = 0 -> ""
  | x -> symb ^ string_of_length symb (length - 1)

let rec row_helper col row offset board =
  if col > 0 then
    let back =
      if (col + offset) mod 2 = 1 then dark_color else light_color
    in
    if row = (snd square_size / 2) + 1 then (
      let piece =
        Game_state.get_square board { rank = offset; file = 9 - col }
      in
      let front =
        match snd piece with
        | White -> white_code
        | Black -> black_code
        | NoPiece -> white_code
      in
      print_color front back
        (string_of_length " " (fst square_size / 2));
      print_color front back (Game_state.get_piece_str piece);
      print_color front back
        (string_of_length " " (fst square_size / 2));
      row_helper (col - 1) row offset board)
    else (
      print_color 0 back (string_of_length " " (fst square_size));
      row_helper (col - 1) row offset board)

let rec square_helper row offset (state : Game_state.game_state) =
  let board = state.board in
  if row > 0 then (
    let row_disp =
      "   "
      ^ (if row = (snd square_size / 2) + 1 then string_of_int offset
        else " ")
      ^ "   "
    in
    print_string row_disp;
    row_helper 8 row offset board;
    let sidebar =
      get_sidebar_row state (((offset - 1) * snd square_size) + row)
    in
    print_string (row_disp ^ sidebar ^ "\n");
    square_helper (row - 1) offset state)

let draw_row offset state = square_helper (snd square_size) offset state

let rec board_helper row state =
  if row > 0 then (
    draw_row row state;
    board_helper (row - 1) state;
    ())

let update_display (curr_state : Game_state.game_state) =
  let half_spacer = string_of_length " " (fst square_size / 2) in
  let spacer = string_of_length " " (fst square_size - 1) in
  let top_row =
    "       " ^ half_spacer ^ "A" ^ spacer ^ "B" ^ spacer ^ "C" ^ spacer
    ^ "D" ^ spacer ^ "E" ^ spacer ^ "F" ^ spacer ^ "G" ^ spacer ^ "H"
    ^ "\n"
  in
  print_endline top_row;
  board_helper 8 curr_state;
  print_endline ("\n" ^ top_row);
  let turn =
    match Game_state.color_to_move curr_state.board with
    | White -> "White"
    | Black -> "Black"
    | NoPiece -> failwith "Invalid player color"
  in
  print_endline (turn ^ " to move!")

let show_start =
  print_endline "CS 3110 presents";
  print_color light_color black_code
    "\n\
    \          _____                    _____                    \
     _____                    _____                    _____          \n\
    \         /\\    \\                  /\\    \\                  \
     /\\    \\                  /\\    \\                  /\\    \
     \\         \n\
    \        /::\\    \\                /::\\____\\                \
     /::\\    \\                /::\\    \\                /::\\    \
     \\        \n\
    \       /::::\\    \\              /:::/    /               \
     /::::\\    \\              /::::\\    \\              /::::\\    \
     \\       \n\
    \      /::::::\\    \\            /:::/    /               \
     /::::::\\    \\            /::::::\\    \\            \
     /::::::\\    \\      \n\
    \     /:::/\\:::\\    \\          /:::/    /               \
     /:::/\\:::\\    \\          /:::/\\:::\\    \\          \
     /:::/\\:::\\    \\     \n\
    \    /:::/  \\:::\\    \\        /:::/____/               \
     /:::/__\\:::\\    \\        /:::/__\\:::\\    \\        \
     /:::/__\\:::\\    \\    \n\
    \   /:::/    \\:::\\    \\      /::::\\    \\              \
     /::::\\   \\:::\\    \\       \\:::\\   \\:::\\    \\       \
     \\:::\\   \\:::\\    \\   \n\
    \  /:::/    / \\:::\\    \\    /::::::\\    \\   _____    \
     /::::::\\   \\:::\\    \\    ___\\:::\\   \\:::\\    \\    \
     ___\\:::\\   \\:::\\    \\  \n\
    \ /:::/    /   \\:::\\    \\  /:::/\\:::\\    \\ /\\    \\  \
     /:::/\\:::\\   \\:::\\    \\  /\\   \\:::\\   \\:::\\    \\  \
     /\\   \\:::\\   \\:::\\    \\ \n\
     /:::/____/     \\:::\\____\\/:::/  \\:::\\    \
     /::\\____\\/:::/__\\:::\\   \\:::\\____\\/::\\   \\:::\\   \
     \\:::\\____\\/::\\   \\:::\\   \\:::\\____\\ \n\
     \\:::\\    \\      \\::/    /\\::/    \\:::\\  /:::/    \
     /\\:::\\   \\:::\\   \\::/    /\\:::\\   \\:::\\   \\::/    \
     /\\:::\\   \\:::\\   \\::/    / \n\
    \ \\:::\\    \\      \\/____/  \\/____/ \\:::\\/:::/    /  \
     \\:::\\   \\:::\\   \\/____/  \\:::\\   \\:::\\   \\/____/  \
     \\:::\\   \\:::\\   \\/____/ \n\
    \  \\:::\\    \\                       \\::::::/    /    \\:::\\   \
     \\:::\\    \\       \\:::\\   \\:::\\    \\       \\:::\\   \
     \\:::\\    \\     \n\
    \   \\:::\\    \\                       \\::::/    /      \
     \\:::\\   \\:::\\____\\       \\:::\\   \\:::\\____\\       \
     \\:::\\   \\:::\\____\\    \n\
    \    \\:::\\    \\                      /:::/    /        \
     \\:::\\   \\::/    /        \\:::\\  /:::/    /        \\:::\\  \
     /:::/    /    \n\
    \     \\:::\\    \\                    /:::/    /          \
     \\:::\\   \\/____/          \\:::\\/:::/    /          \
     \\:::\\/:::/    /     \n\
    \      \\:::\\    \\                  /:::/    /            \
     \\:::\\    \\               \\::::::/    /            \
     \\::::::/    /      \n\
    \       \\:::\\____\\                /:::/    /              \
     \\:::\\____\\               \\::::/    /              \\::::/    \
     /       \n\
    \        \\::/    /                \\::/    /                \
     \\::/    /                \\::/    /                \\::/    \
     /        \n\
    \         \\/____/                  \\/____/                  \
     \\/____/                  \\/____/                  \
     \\/____/         ";
  print_endline "\n\nPress enter to begin...";
  let _ = read_line () in
  print_endline "Welcome!"

let show_end over =
  if over then
    print_color light_color black_code
      "\n\
      \  ___           ___           ___           \
       ___                    ___                        ___           \
       ___     \n\
      \  /  /\\         /  /\\         /__/\\         /  \
       /\\                  /  /\\          ___         /  /\\         \
       /  /\\    \n\
      \ /  /:/_       /  /::\\       |  |::\\       /  \
       /:/_                /  /::\\        /__/\\       /  /:/_       \
       /  /::\\   \n\
       /  /:/ /\\     /  /:/\\:\\      |  |:|:\\     /  /:/ \
       /\\              /  /:/\\:\\       \\  \\:\\     /  /:/ /\\     \
       /  /:/\\:\\  \n\
       /  /:/_/::\\   /  /:/~/::\\   __|__|:|\\:\\   /  /:/ \
       /:/_            /  /:/  \\:\\       \\  \\:\\   /  /:/ /:/_   \
       /  /:/~/:/  \n\
       /__/:/__\\/\\:\\ /__/:/ /:/\\:\\ /__/::::| \\:\\ /__/:/ /:/ \
       /\\          /__/:/ \\__\\:\\  ___  \\__\\:\\ /__/:/ /:/ /\\ \
       /__/:/ /:/___\n\
       \\  \\:\\ /~~/:/ \\  \\:\\/:/__\\/ \\  \\:\\~~\\__\\/ \\  \
       \\:\\/:/ /:/          \\  \\:\\ /  /:/ /__/\\ |  |:| \\  \
       \\:\\/:/ /:/ \\  \\:\\/:::::/\n\
       \\  \\:\\  /:/   \\  \\::/       \\  \\:\\        \\  \\::/ \
       /:/            \\  \\:\\  /:/  \\  \\:\\|  |:|  \\  \\::/ /:/   \
       \\  \\::/~~~~ \n\
       \\  \\:\\/:/     \\  \\:\\        \\  \\:\\        \\  \
       \\:\\/:/              \\  \\:\\/:/    \\  \\:\\__|:|   \\  \
       \\:\\/:/     \\  \\:\\     \n\
      \ \\  \\::/       \\  \\:\\        \\  \\:\\        \\  \
       \\::/                \\  \\::/      \\__\\::::/     \\  \
       \\::/       \\  \\:\\    \n\
      \  \\__\\/         \\__\\/         \\__\\/         \
       \\__\\/                  \\__\\/           ~~~~       \
       \\__\\/         \\__\\/    \n\
      \  \n";
  print_endline "You lose!"

let instructions () =
  print_color light_color black_code
    "\n\
    \  This is a two player game. \n\n\
    \  There are kinds of pieces:\n\
    \  Pawn: ♟︎\n\
    \  1.  On a Pawn’s first move, it can move forward one or two \
     squares.\n\
    \  2.  When capturing a piece (see description on back), a Pawn \
     moves one square diagonally ahead.\n\
    \  \n\
    \  Knight: ♞ \n\
    \  1.  Knights move three squares at a time: two spaces forward or \
     backward, then one space left or right, or two spaces to the \t\n\
    \  left or right, then one space forward or backward. \n\
    \  2.  The move looks like the letter L. The Knight always ends up \
     landing on a square opposite the color from which it started.\n\
    \  \n\
    \  Bishop: ♝\n\
    \  1.  Bishops moves diagonally as many open squares as you like. \n\
    \  2.  The Bishop must remain on the same color square as it \
     started the game on. \n\
    \  \n\
    \  Rook: ♜\n\
    \  1.  Rook moves in a straight line, horizontally or vertically \
     as many open squares as you like.\n\
    \  Besides the Queen, the Rook is the next most powerful Play Piece\n\
    \  \n\
    \  Queen: ♛\n\
    \  1.  Queen is the most powerful of the Play Pieces. \n\
    \  2.  The Queen moves in any direction (horizontally, vertically \
     or diagonally) as many open squares as you like. \n\
    \  \n\
    \  King: ♚\n\
    \  1.  King is the most important Play Piece, because if it \
     becomes trapped, you’ll lose the game.\n\
    \  2.  The King moves one square in any direction, as long as it \
     doesn’t put itself in Check\n\n\
    \  Check: \n\
    \  You are in Check if an opponent’s Play Piece is in a position \
     on the board to capture your King.\n\
    \  To save your King from Check you must do one of the following:\n\
    \  1. Move the King out of the way of the opponent’s Play Piece\n\
    \  2. Move another one of your Play Pieces to block your opponent\n\
    \  3. Capture the opponent’s Play Piece that is threatening your \
     King\n\n\
    \  Checkmate: ♚\n\
    \  When your King cannot be saved from Check, it’s called \
     Checkmate and you lost the game. \n\n\
    \  Capturing:\n\
    \  When you move one of your Play Pieces and it ends on an \
     opponent’s Play Piece, you capture\n\
    \  it and remove it from the Game Board\n\n\
    \  Castling: ♜♚ -> ♚♜\n\
    \  This is a special move for the King and either Rook and is the \
     only time two Play Pieces can\n\
    \  move on one turn. It helps to protect the King and positions \
     the Rook toward the center of the Game Board.\n\
    \  To castle, slide the Rook to the space next to the King. Move \
     the King to the other side of the Rook. \n\
    \  To castle:\n\
    \  1. This must be the first move for both the King and the Rook.\n\
    \  2. No other Play Pieces can be between the King and the Rook.\n\
    \  3. The King can’t be in Check, either before or after the \
     castle. \n\
    \  4. The King can’t be in Check on any of the spaces that it \
     passes over during the castle.\n\n\
    \  Winning: |♚|\n\
    \  If you put your opponent’s King in Check so he can’t escape, \
     call Checkmate… you win!\n\
    \  \n\
     Let's play!\n\n\
    \      Press [ENTER] to continue.\n\
    \        ";
  let _ = read_line () in
  ()