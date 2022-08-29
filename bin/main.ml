open Chess
(** Code to start gameplay will go here *)

open Unix

let start_board =
  Game_state.get_board_from_FEN
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let start_state : Game_state.game_state =
  {
    board = start_board;
    white_taken = [];
    black_taken = [];
    time = (300, 300);
  }

let rec prompt_fen () =
  print_endline "Enter a FEN string: ";
  let new_fen = read_line () in
  try { start_state with board = Game_state.get_board_from_FEN new_fen }
  with Failure s ->
    print_endline "Invalid FEN string. Try again";
    prompt_fen ()

let rec prompt_time () =
  print_endline "Time control:\n How many seconds does white have?";
  let white_time = read_line () in
  print_endline "How many seconds does\n  black have?";
  let black_time = read_line () in
  try
    {
      start_state with
      time = (int_of_string white_time, int_of_string black_time);
    }
  with Failure s ->
    print_endline "Invalid FEN string. Try again";
    prompt_fen ()

let rec prompt_options () =
  print_endline
    "\n\
    \ What whould you like to do?\n\
    \ [PLAY] start a new game\n\
    \ [LOAD] load a game from a FEN string\n\
    \ [OPTIONS] play using nonstandard options (ie setting time control)\n\
    \ [HOW TO] learn how to play the game\n\
    \      ";
  let res = read_line () in
  match String.lowercase_ascii res with
  | "play" -> start_state
  | "load" -> prompt_fen ()
  | "options" -> prompt_time ()
  | "how to" ->
      Ui.instructions ();
      prompt_options ()
  | _ ->
      print_endline "Please choose one of the given options.";
      prompt_options ()

let turn_swap result =
  match result with
  | x, Game_state.Legal, p -> (Game_state.swap_turn x, p)
  | x, Game_state.Illegal, p ->
      print_endline "Illegal move, please enter new move.";
      (x, p)

let update_state (state : Game_state.game_state) new_board taken =
  match taken with
  | Game_state.Empty, _ -> { state with board = new_board }
  | x -> (
      match x with
      | p, Game_state.White ->
          {
            state with
            board = new_board;
            white_taken = p :: state.white_taken;
          }
      | p, Game_state.Black ->
          {
            state with
            board = new_board;
            black_taken = p :: state.black_taken;
          }
      | _ -> failwith "Cannot add piece of invalid color to taken.")

let rec gameplay_loop (state : Game_state.game_state) =
  let _ = Ui.update_display state in
  let t = Unix.gettimeofday () in
  let move = Gameplay.take_move "" in
  if Gameplay.check_end move then
    let move_result =
      Move_validation.attempt_move state.board
        (Gameplay.return_start move true)
        (Gameplay.return_start move false)
    in
    let exec_time = Unix.gettimeofday () -. t in
    let remain_time =
      Gameplay.print_time
        (Game_state.color_to_move state.board)
        state
        (int_of_float exec_time)
    in
    if Gameplay.still_time remain_time then
      let result = turn_swap move_result in
      let new_board = fst result in
      let taken_piece = snd result in
      let new_state = update_state state new_board taken_piece in
      gameplay_loop (Game_state.set_time new_state remain_time)
    else Ui.show_end true
  else Ui.show_end true

let _ = Ui.show_start

let _ = gameplay_loop (prompt_options ())
