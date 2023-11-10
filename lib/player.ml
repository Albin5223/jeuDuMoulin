open Type
open Board

let init_player (c : Type.color) : player =
    { phase = Placing; color = c; bag = []; piece_placed = 0; nb_pieces_on_board = 0 }

(** The max amount of pieces that a player can hold *)
let max_pieces = 9

(**This function return a bool if the player can't move *)
let cant_move (player : player) (game : game_update) : bool =
    let rec aux (player : player) (game : game_update) (bag : coordinates list) : bool =
        match bag with
        | [] -> true
        | (x, y) :: xs -> List.length (possible_moves game (x, y) player.color) = 0 && aux player game xs
    in
    aux player game player.bag

let rec play_randomly random (player : color) (game : game_update) (current_phase : phase) : game_update =
    if current_phase = Placing
    then
      let i = random board_size in
      let j = random board_size in
      (*print_string ("I , J -> "^string_of_int i ^"; "^string_of_int j^"\n");*)
      let tmp = place_start_piece game (i, j) player in
      (*(if tmp.mill then print_string ("MILLLLL"^ string_of_int (get_player tmp player).nb_pieces_on_board^(string_of_int (get_opponent tmp player).nb_pieces_on_board) ^"\n") else print_string "No milll \n");*)
      if tmp.mill
      then
        let pieceCC = List.nth (get_opponent tmp player).bag (random (get_opponent tmp player).nb_pieces_on_board) in
        let newgame = eliminate_piece tmp pieceCC (get_opponent tmp player).color in
        newgame
      else if not tmp.game_is_changed
      then
        play_randomly random player game
          current_phase (*if we choose coordinates where a piece is already here or a path or a wall*)
      else tmp
    else if current_phase = Moving
    then
      (*either the bot can just move or the opponent is flying but not the bot*)
      let x, y = List.nth (get_player game player).bag (random (get_player game player).nb_pieces_on_board) in
      let movesPossible = possible_moves game (x, y) player false in
      if List.length movesPossible = 0
      then play_randomly random player game current_phase (*if we have a unmovable piece, we examine another piece*)
      else
        let tmp = move_to_direction game (x, y) (List.nth movesPossible (random (List.length movesPossible))) player in
        if tmp.mill
        then
          let pieceCC = List.nth (get_opponent tmp player).bag (random (get_opponent tmp player).nb_pieces_on_board) in
          let newgame = eliminate_piece tmp pieceCC (get_opponent tmp player).color in
          newgame
        else tmp
    else
      (*this means either the bot is flying or both players are flying*)
      let i = random board_size in
      let j = random board_size in
      let tmp =
          move_to_coordinates game
            (List.nth (get_player game player).bag (random (get_player game player).nb_pieces_on_board))
            (i, j) player
      in
      if tmp.mill
      then
        let pieceCC = List.nth (get_opponent tmp player).bag (random (get_opponent tmp player).nb_pieces_on_board) in
        let newgame = eliminate_piece tmp pieceCC (get_opponent tmp player).color in
        newgame
      else if not tmp.game_is_changed
      then
        play_randomly random player game
          current_phase (*if we choose coordinates where a piece is already here or a path or a wall*)
      else tmp

let lost (game : game_update) (player : player) : bool =
    if player.nb_pieces_on_board <= 2 && player.piece_placed = 9
    then true
    else
      match player.phase with
      | Moving | Flying -> cant_move player game
      | _ -> false
