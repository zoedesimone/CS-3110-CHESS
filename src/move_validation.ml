open Game_state
open Piece

(**[color_eq a b] returns true if two colors are the same *)
let color_eq a b = (a = Black && b = Black) || (a = White && b = White)

(**[same_first a b] returns true if the first element of two tuples are
   the same*)
let same_first a b = fst a = fst b

(**[same_second a b] returns true if the second element of two tuples
   are the same*)
let same_second a b = snd a = snd b

(**[equal_tuple a b] returns true if the both elements of two tuples are
   the same*)
let equal_tuple a b = same_first a b && same_second a b

let string_of_piece piece =
  match piece with
  |King -> "K"
  |Queen -> "Q"
  |Bishop -> "B"
  |Knight -> "N"
  |Rook -> "R"
  |Pawn -> "P"
  |Empty -> "empty"

let string_of_color color =
  match color with
  |White -> "White"
  |Black -> "Black"
  |NoPiece -> "NP"

let piece_string a =
  (string_of_piece (fst a)) ^ " " ^ string_of_color (snd a)

let rec pieces_in_between_same_rank_helper board s f =
  match (s.file,f.file) with
  |(a,b) when a>b -> get_square board s :: pieces_in_between_same_rank_helper board {rank = s.rank; file = s.file-1} f
  |(a,b) when a<b -> get_square board s :: pieces_in_between_same_rank_helper board {rank = s.rank; file = s.file+1} f
  |(a,b) when a = b -> []
  |(_,_) -> failwith("pieces_in_between_same_rank failure")

let pieces_in_between_same_rank board start finish= try List.tl (pieces_in_between_same_rank_helper board start finish) with Failure _ ->[]


let rec pieces_in_between_same_file_helper board s f =
  match (s.rank,f.rank) with
  |(a,b) when a>b -> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank -1; file = s.file} f
  |(a,b) when a<b -> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank + 1; file = s.file} f
  |(a,b) when a = b -> []
  |(_,_) -> failwith("pieces_in_between_same_file failure")

let pieces_in_between_same_file board start finish= try List.tl (pieces_in_between_same_file_helper board start finish) with Failure _ ->[]

let rec pieces_in_between_diagonal_helper board s f =
  match (s.rank,f.rank,s.file,f.file) with
  |(a,b,c,d) when a>b && c>d-> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank -1; file = s.file -1} f
  |(a,b,c,d) when a<b && c<d-> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank +1; file = s.file +1} f
  |(a,b,c,d) when a>b && c<d-> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank -1; file = s.file +1} f
  |(a,b,c,d) when a<b && c>d-> get_square board s :: pieces_in_between_same_file_helper board {rank = s.rank +1; file = s.file -1} f
  |(a,b,c,d) when a = b && c =d -> []
  |(_,_,_,_) -> failwith("pieces_in_between_diagonal failure")

let pieces_in_between_diagonal board start finish = try List.tl (pieces_in_between_diagonal_helper board start finish) with Failure _ ->[]


(**[pieces_in_between board start finish] returns the pieces in between start and finish if a knight is chosen as start, the empty list is returned, otherwise if a pieces is not on the same rank, file, or diagonal, a failure happens*)
let pieces_in_between board start finish =
  (*if start is a knight*)
  if fst (get_square board start) = Knight then [] (*if rank is same*)
  else if start.rank = finish.rank then
    pieces_in_between_same_rank board start finish (*if file is same*)
  else if start.file = finish.file then
    pieces_in_between_same_file board start finish (*diagonal*)
  else pieces_in_between_diagonal board start finish

(**[clear_path pieces] returns true if pieces contains all Empty pieces
   of is the empty string*)
let rec clear_path pieces =
  match pieces with
  | [] -> true
  | (a, b) :: t when a = Empty -> true && clear_path t
  | (a, b) :: t when a <> Empty -> false
  | _ -> failwith "clear_path failure"

(**[friendly_fire board start finish] returns true if start and finish
   hold pieces of the same color*)
let friendly_fire board start finish =
  let the_piece = Game_state.get_square board start in
  let target_piece = Game_state.get_square board finish in
  if fst target_piece = Empty then false
  else
    (snd the_piece = Black && snd target_piece = Black)
    || (snd the_piece = White && snd target_piece = White)

let pawn_legality board start finish =
  (*white goes up *)
  if snd (Game_state.get_square board start) = White && finish.rank<start.rank then false 
  (*Black goes down *)
  else if snd (Game_state.get_square board start) = Black && finish.rank>start.rank then false
  (*Can only move by two at start*)
  else if (finish.rank-start.rank = 2 && start.rank <> 2)  || (start.rank-finish.rank = 2 && start.rank <> 7) then false
  (*Can only move diagonally if theres a piece there*)
  else if start.file<>finish.file &&  fst (Game_state.get_square board finish) = Empty then false
  (*Can't take pieces moving vertically *)
  else if start.file=finish.file && fst (Game_state.get_square board finish) <> Empty then false
  else let possible_moves = Piece.get_moves (fst (get_square board start))  (start.file,start.rank) in
    List.exists (equal_tuple (finish.file,finish.rank)) possible_moves

let piece_legality board start finish = 
  (*et moves_list = get_moves (fst (get_square board start)) (start.rank,start.finish) in *)
  if  fst (Game_state.get_square board start) = Pawn then pawn_legality board start finish else
    let possible_moves = Piece.get_moves (fst (get_square board start))  (start.file,start.rank) in
    List.exists (equal_tuple (finish.file,finish.rank)) possible_moves

let opposite_color color =
  match color with
  | White -> Black
  | Black -> White
  | NoPiece -> failwith "opposite color failure"

let check_from_knight board king_square =
  let k_rank = king_square.rank in
  let k_file = king_square.file in
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let a = if k_rank <= 6 && k_file<=7 then Game_state.get_square board {rank = k_rank + 2; file = k_file + 1} = (Knight, opp_color)
    else false in
  let b = if k_rank <= 6 && k_file>=2 then Game_state.get_square board  {rank = k_rank + 2; file = k_file - 1} = (Knight, opp_color)
    else false in
  let c = if k_rank >= 3 && k_file<=7 then Game_state.get_square board  {rank = k_rank - 2; file = k_file + 1} = (Knight, opp_color)
    else false in
  let d = if k_rank >= 3 && k_file>=2 then Game_state.get_square board  {rank = k_rank - 2; file = k_file - 1} = (Knight, opp_color)
    else false in
  let e = if k_rank <= 7 && k_file<=6 then Game_state.get_square board  {rank = k_rank + 1; file = k_file + 2} = (Knight, opp_color)
    else false in
  let f = if k_rank <= 7 && k_file>=3 then Game_state.get_square board  {rank = k_rank + 1; file = k_file - 2} = (Knight, opp_color)
    else false in
  let g = if k_rank >= 2 && k_file<=6 then Game_state.get_square board  {rank = k_rank - 1; file = k_file + 2} = (Knight, opp_color)
    else false in
  let h = if k_rank >= 2 && k_file>=3 then Game_state.get_square board  {rank = k_rank - 1; file = k_file - 2} = (Knight, opp_color)
    else false in
  a||b||c||d||e||f||g||h

let rec first_non_empty_piece_in_list alist =
  match alist with
  | h :: t ->
      if fst h = Empty then first_non_empty_piece_in_list t else h
  | [] -> (Empty, NoPiece)

let check_above_king board king_square =
  let k_file = king_square.file in
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let threat_piece =
    first_non_empty_piece_in_list
      (pieces_in_between_same_file board king_square
         { rank = 9; file = k_file })
  in
  match threat_piece with
  | Queen, c -> if c = opp_color then true else false
  | Rook, d -> if d = opp_color then true else false
  | _ -> false

let check_below_king board king_square =
  let k_file = king_square.file in
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let threat_piece =
    first_non_empty_piece_in_list
      (pieces_in_between_same_file board king_square
         { rank = 0; file = k_file })
  in
  match threat_piece with
  | Queen, c -> if c = opp_color then true else false
  | Rook, d -> if d = opp_color then true else false
  | _ -> false

let check_left_king board king_square =
  let k_rank = king_square.rank in
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let threat_piece =
    first_non_empty_piece_in_list
      (pieces_in_between_same_rank board king_square
         { rank = k_rank; file = 0 })
  in
  match threat_piece with
  | Queen, c -> if c = opp_color then true else false
  | Rook, d -> if d = opp_color then true else false
  | _ -> false

let check_right_king board king_square =
  let k_rank = king_square.rank in
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let threat_piece =
    first_non_empty_piece_in_list
      (pieces_in_between_same_rank board king_square
         { rank = k_rank; file = 9 })
  in
  match threat_piece with
  | Queen, c -> if c = opp_color then true else false
  | Rook, d -> if d = opp_color then true else false
  | _ -> false

let rec diagonal_end_finder_up_right coord =
  if coord.rank >= 8 || coord.file >= 8 then coord
  else
    diagonal_end_finder_up_right
      { rank = coord.rank + 1; file = coord.file + 1 }

let rec diagonal_end_finder_up_left coord =
  if coord.rank >= 8 || coord.file <= 0 then coord
  else
    diagonal_end_finder_up_left
      { rank = coord.rank + 1; file = coord.file - 1 }

let rec diagonal_end_finder_down_right coord =
  if coord.rank <= 1 || coord.file >= 8 then coord
  else
    diagonal_end_finder_down_right
      { rank = coord.rank - 1; file = coord.file + 1 }

let rec diagonal_end_finder_down_left coord =
  if coord.rank <= 1 || coord.file <= 1 then coord
  else
    diagonal_end_finder_down_left
      { rank = coord.rank - 1; file = coord.file - 1 }

let check_up_right_king board king_square =
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let threat_piece = first_non_empty_piece_in_list (pieces_in_between_diagonal board king_square (diagonal_end_finder_up_right king_square)) in
  (*print_string(piece_string threat_piece);*)
  match threat_piece with
  | Queen, c -> if c = opp_color then true else false
  | Bishop, d -> if d = opp_color then true else false
  | _ -> false

let check_up_left_king board king_square =
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let threat_piece =
    first_non_empty_piece_in_list
      (pieces_in_between_diagonal board king_square
         (diagonal_end_finder_up_left king_square))
  in
  match threat_piece with
  | Queen, c -> if c = opp_color then true else false
  | Bishop, d -> if d = opp_color then true else false
  | _ -> false

let check_down_left_king board king_square =
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let threat_piece =
    first_non_empty_piece_in_list
      (pieces_in_between_diagonal board king_square
         (diagonal_end_finder_down_left king_square))
  in
  match threat_piece with
  | Queen, c -> if c = opp_color then true else false
  | Bishop, d -> if d = opp_color then true else false
  | _ -> false

let check_down_right_king board king_square =
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let threat_piece =
    first_non_empty_piece_in_list
      (pieces_in_between_diagonal board king_square
         (diagonal_end_finder_down_right king_square))
  in
  match threat_piece with
  | Queen, c -> if c = opp_color then true else false
  | Bishop, d -> if d = opp_color then true else false
  | _ -> false

let check_from_king board king_square =
  let k_rank = king_square.rank in
  let k_file = king_square.file in
  let opp_color = opposite_color (Game_state.color_to_move board) in
  let a = if k_rank <= 7 && k_file <= 7 then Game_state.get_square board {rank = k_rank + 1; file = k_file + 1} = (King, opp_color)
    else false in
  let b = if k_rank <= 7 && k_file <= 8 then Game_state.get_square board {rank = k_rank + 1; file = k_file + 0} = (King, opp_color)
    else false in
  let c = if k_rank <= 7 && k_file >= 2 then Game_state.get_square board {rank = k_rank + 1; file = k_file - 1} = (King, opp_color)
    else false in
  let d = if k_rank <= 8 && k_file >= 2 then Game_state.get_square board {rank = k_rank + 0; file = k_file - 1} = (King, opp_color)
    else false in
  let e = if k_rank >= 2 && k_file >= 2 then Game_state.get_square board {rank = k_rank - 1; file = k_file - 1} = (King, opp_color)
    else false in
  let f = if k_rank >= 2 && k_file <= 8 then Game_state.get_square board {rank = k_rank - 1; file = k_file + 0} = (King, opp_color)
    else false in
  let g = if k_rank >= 2 && k_file <= 7 then Game_state.get_square board {rank = k_rank - 1; file = k_file + 1} = (King, opp_color)
    else false in
  let h = if k_rank <= 8 && k_file <= 7 then Game_state.get_square board {rank = k_rank - 0; file = k_file + 1} = (King, opp_color)
    else false in
  a||b||c||d||e||f||g||h

let check_from_pawn board king_square =
  let k_rank = king_square.rank in
  let k_file = king_square.file in
  let color = Game_state.color_to_move board in
  match color with
  |White ->
    let a = if k_rank <= 7 && k_file <= 7 then  Game_state.get_square board {rank = k_rank + 1; file = k_file + 1} = (Pawn, Black)
      else false in
    let b = if k_rank <= 7 && k_file >= 2 then  Game_state.get_square board {rank = k_rank + 1; file = k_file - 1} = (Pawn, Black)
      else false in
    a||b
  |Black -> 
    let a = if k_rank >= 2 && k_file >= 2 then  Game_state.get_square board {rank = k_rank - 1; file = k_file - 1} = (Pawn, White)
      else false in
    let b = if k_rank >= 2 && k_file >= 2 then  Game_state.get_square board {rank = k_rank - 1; file = k_file + 1} = (Pawn, White)
      else false in
    a||b

  |NoPiece -> failwith("check from pawn error")

let in_check board king_square = (*
  check_down_right_king board king_square ||
  check_down_left_king board king_square || 
  check_up_left_king board king_square ||*)
  (*check_up_right_king board king_square ||*)
  check_from_pawn board king_square||
  check_from_king board king_square || 
  check_from_knight board king_square ||
  check_below_king board king_square || 
  check_above_king board king_square || 
  check_right_king board king_square || 
  check_left_king board king_square 
let right_color board start = 
  let the_piece = Game_state.get_square board start in
  snd the_piece = Game_state.color_to_move board

let white_king_moved = ref false

let white_queen_rook_moved = ref false

let white_king_rook_moved = ref false

let black_king_moved = ref false

let black_queen_rook_moved = ref false

let black_king_rook_moved = ref false

let update_castle_availability_white board start finish =
  if Game_state.get_square board { rank = 1; file = 8 } <> (Rook, White)
  then white_king_rook_moved := true;
  if Game_state.get_square board { rank = 1; file = 1 } <> (Rook, White)
  then white_queen_rook_moved := true;
  if Game_state.get_square board { rank = 1; file = 5 } <> (King, White)
  then white_king_moved := true;
  let b1 = Game_state.get_square board { rank = 1; file = 2 } in
  let c1 = Game_state.get_square board { rank = 1; file = 3 } in
  let d1 = Game_state.get_square board { rank = 1; file = 4 } in
  let check_castle_queen =
    fst b1 = Empty
    && fst c1 = Empty
    && fst d1 = Empty
    && (not !white_king_moved)
    && not !white_queen_rook_moved
  in
  let f1 = Game_state.get_square board { rank = 1; file = 6 } in
  let g1 = Game_state.get_square board { rank = 1; file = 7 } in
  let check_castle_king =
    fst f1 = Empty
    && fst g1 = Empty
    && (not !white_king_moved)
    && not !white_king_rook_moved
  in
  Game_state.set_castle_availability board White
    { king_side = check_castle_king; queen_side = check_castle_queen }

let update_castle_availability_black board start finish =
  if Game_state.get_square board { rank = 8; file = 8 } <> (Rook, Black)
  then black_king_rook_moved := true;
  if Game_state.get_square board { rank = 8; file = 1 } <> (Rook, Black)
  then black_queen_rook_moved := true;
  if Game_state.get_square board { rank = 8; file = 5 } <> (King, Black)
  then black_king_moved := true;
  let b8 = Game_state.get_square board { rank = 8; file = 2 } in
  let c8 = Game_state.get_square board { rank = 8; file = 3 } in
  let d8 = Game_state.get_square board { rank = 8; file = 4 } in
  let check_castle_queen =
    fst b8 = Empty
    && fst c8 = Empty
    && fst d8 = Empty
    && (not !black_king_moved)
    && not !black_queen_rook_moved
  in
  let f8 = Game_state.get_square board { rank = 8; file = 6 } in
  let g8 = Game_state.get_square board { rank = 8; file = 7 } in
  let check_castle_king =
    fst f8 = Empty
    && fst g8 = Empty
    && (not !black_king_moved)
    && not !black_king_rook_moved
  in
  Game_state.set_castle_availability board Black
    { king_side = check_castle_king; queen_side = check_castle_queen }

let update_castle_availability board start finish =
  if Game_state.color_to_move board = White then
    update_castle_availability_white board start finish
  else update_castle_availability_black board start finish

let detect_castle board start finish =
  let king = Game_state.get_square board start in
  if
    start = { rank = 1; file = 5 }
    && fst king = King
    && (finish = { rank = 1; file = 7 }
       || finish = { rank = 1; file = 3 })
    || start = { rank = 8; file = 5 }
       && fst king = King
       && (finish = { rank = 8; file = 7 }
          || finish = { rank = 8; file = 3 })
  then true
  else false

let castle board start finish =
  if
    finish = { rank = 1; file = 7 }
    && (get_castle_availability board White).king_side
  then
    let aboard =
      Game_state.set_square board { rank = 1; file = 5 } (Empty, NoPiece)
    in
    let bboard =
      Game_state.set_square aboard { rank = 1; file = 8 }
        (Empty, NoPiece)
    in
    let cboard =
      Game_state.set_square bboard { rank = 1; file = 7 } (King, White)
    in
    let dboard =
      Game_state.set_square cboard { rank = 1; file = 6 } (Rook, White)
    in
    (dboard, Game_state.Legal, (Empty, NoPiece))
  else if
    finish = { rank = 1; file = 7 }
    && not (get_castle_availability board White).king_side
  then (board, Game_state.Illegal, (Empty, NoPiece))
  else if
    finish = { rank = 1; file = 3 }
    && (get_castle_availability board White).queen_side
  then
    let aboard =
      Game_state.set_square board { rank = 1; file = 5 } (Empty, NoPiece)
    in
    let bboard =
      Game_state.set_square aboard { rank = 1; file = 1 }
        (Empty, NoPiece)
    in
    let cboard =
      Game_state.set_square bboard { rank = 1; file = 3 } (King, White)
    in
    let dboard =
      Game_state.set_square cboard { rank = 1; file = 4 } (Rook, White)
    in
    (dboard, Game_state.Legal, (Empty, NoPiece))
  else if
    finish = { rank = 1; file = 3 }
    && not (get_castle_availability board White).queen_side
  then (board, Game_state.Illegal, (Empty, NoPiece))
  else if
    finish = { rank = 8; file = 7 }
    && (get_castle_availability board Black).king_side
  then
    let aboard =
      Game_state.set_square board { rank = 8; file = 5 } (Empty, NoPiece)
    in
    let bboard =
      Game_state.set_square aboard { rank = 8; file = 8 }
        (Empty, NoPiece)
    in
    let cboard =
      Game_state.set_square bboard { rank = 8; file = 7 } (King, Black)
    in
    let dboard =
      Game_state.set_square cboard { rank = 8; file = 6 } (Rook, Black)
    in
    (dboard, Game_state.Legal, (Empty, NoPiece))
  else if
    finish = { rank = 8; file = 7 }
    && not (get_castle_availability board Black).king_side
  then (board, Game_state.Illegal, (Empty, NoPiece))
  else if
    finish = { rank = 8; file = 3 }
    && (get_castle_availability board Black).queen_side
  then
    let aboard =
      Game_state.set_square board { rank = 8; file = 5 } (Empty, NoPiece)
    in
    let bboard =
      Game_state.set_square aboard { rank = 8; file = 1 }
        (Empty, NoPiece)
    in
    let cboard =
      Game_state.set_square bboard { rank = 8; file = 3 } (King, Black)
    in
    let dboard =
      Game_state.set_square cboard { rank = 8; file = 4 } (Rook, Black)
    in
    (dboard, Game_state.Legal, (Empty, NoPiece))
  else if
    finish = { rank = 8; file = 3 }
    && not (get_castle_availability board Black).queen_side
  then (board, Game_state.Illegal, (Empty, NoPiece))
  else failwith "castle error"

let detect_promotion board start finish =
  (Game_state.get_square board start = (Pawn, White) && finish.rank = 8)
  || Game_state.get_square board start = (Pawn, Black)
     && finish.rank = 1



let en_passant_update_white board start finish = 
  if  Game_state.get_square board start = (Pawn,White) && start.rank = 2 && finish.rank = 4 
  then Game_state.set_en_passant_target board (Some {rank = 3;file = finish.file})
  (* else failwith (piece_string (Game_state.get_square board start) ^ "srank: " ^ string_of_int start.rank ^ "frank: " ^ string_of_int finish.rank) *)
  else Game_state.set_en_passant_target board None

let en_passant_update_black board start finish =
  if
    Game_state.get_square board start = (Pawn, Black)
    && start.rank = 7 && finish.rank = 5
  then
    Game_state.set_en_passant_target board
      (Some { rank = 6; file = finish.file })
  else Game_state.set_en_passant_target board None

let en_passant_update board start finish =
  match color_to_move board with
  | White -> en_passant_update_white board start finish
  | Black -> en_passant_update_black board start finish
  | _ -> failwith "en_passant_update error"

let start_rank_en_passant board =
  match Game_state.color_to_move board with
  | White -> 5
  | Black -> 4
  | _ -> failwith "start_rank_en_passant error"

let correct_start_file_en_passant start target =
  start.file = target.file + 1 || start.file = target.file - 1

let detect_en_passant board start finish =
  match Game_state.get_en_passant_target board with
  | None -> false
  | Some target ->
      target = finish
      && fst (Game_state.get_square board start) = Pawn
      && start.rank = start_rank_en_passant board
      && correct_start_file_en_passant start target

let move_is_legal board start finish =
  ((piece_legality board start finish
   || detect_castle board start finish)
   && (not (friendly_fire board start finish))
   && clear_path (pieces_in_between board start finish)
  || detect_en_passant board start finish)
  && right_color board start

(** [attempt_move_no_checks board start finish] creates the board
    assuming the move is valid*)
let attempt_move_no_checks board start finish =
  if detect_castle board start finish then castle board start finish
  else
    let the_piece = Game_state.get_square board start in
    let board_with_updated_en_passant =
      en_passant_update board start finish
    in
    let board_with_piece_removed =
      Game_state.set_square board_with_updated_en_passant start
        (Empty, NoPiece)
    in
    let final_board =
      Game_state.set_square board_with_piece_removed finish the_piece
    in
    let the_king =
      Game_state.get_king final_board (Game_state.color_to_move board)
    in
    if not (in_check final_board the_king) then
      (final_board, Game_state.Legal, Game_state.get_square board finish)
    else (board, Game_state.Illegal, (Empty, NoPiece))

let attempt_move_no_checks_then_promote board start finish piece =
  if detect_castle board start finish then castle board start finish
  else
    let board_with_updated_en_passant =
      en_passant_update board start finish
    in

    let the_piece = piece in
    let board_with_piece_removed =
      Game_state.set_square board_with_updated_en_passant start
        (Empty, NoPiece)
    in
    let final_board = Game_state.set_square board_with_piece_removed finish the_piece in
    let the_king = Game_state.get_king final_board (Game_state.color_to_move board) in
    if (not (in_check final_board the_king)) then
      (final_board,Game_state.Legal,Game_state.get_square board finish)
    else (board,Game_state.Illegal,(Empty, NoPiece))


let attempt_move_no_checks_en_passant board start finish =
  let the_piece = Game_state.get_square board start in
  let board_with_updated_en_passant =
    en_passant_update board start finish
  in

  let board_with_attacker_removed =
    Game_state.set_square board_with_updated_en_passant start
      (Empty, NoPiece)
  in
  let board_without_victim =
    Game_state.set_square board_with_attacker_removed
      { rank = start.rank; file = finish.file }
      (Empty, NoPiece)
  in
  let final_board = Game_state.set_square board_without_victim finish the_piece in
  let the_king = Game_state.get_king final_board (Game_state.color_to_move board) in
  if (not (in_check final_board the_king)) then
    (final_board,Game_state.Legal,Game_state.get_square board {rank=start.rank;file = finish.file})
  else (board,Game_state.Illegal,(Empty, NoPiece))



let rec ask_promotion board =
  print_string
    "You are attempting to promote a Pawn. Choose a piece: type Q, R, \
     B, or N for a Queen, Rook, Bishop, or Knight";
  let promoted_string = read_line () in
  match promoted_string with
  | "Q" -> (Queen, Game_state.color_to_move board)
  | "R" -> (Rook, Game_state.color_to_move board)
  | "B" -> (Bishop, Game_state.color_to_move board)
  | "N" -> (Knight, Game_state.color_to_move board)
  | _ -> ask_promotion board

let check_target board =
  match Game_state.get_en_passant_target board with
  | None -> print_string "target = None"
  | Some x ->
      print_string
        ("target rank = " ^ string_of_int x.rank ^ "target file = "
       ^ string_of_int x.file)

(**[attempt_move board board_coord board_coord] validates all board
   considerations (checks, blocked pieces, castling ect.)
   Returns[board, Legal] if the move is allowed*)
let attempt_move board start finish =
  if move_is_legal board start finish then
    if detect_promotion board start finish then
      let promote_piece = ask_promotion board in
      let board_castle_availablility_adjusted =
        update_castle_availability board start finish
      in
      attempt_move_no_checks_then_promote
        board_castle_availablility_adjusted start finish promote_piece
    else if detect_en_passant board start finish then
      attempt_move_no_checks_en_passant board start finish
    else
      let board_castle_availablility_adjusted =
        update_castle_availability board start finish
      in
      attempt_move_no_checks board_castle_availablility_adjusted start
        finish
  else (board, Game_state.Illegal, (Empty, NoPiece))

(*let attempt_move = attempt_move_no_checks*)
