open Type
open Board
open Player

exception Not_Allowed of string

let init_player_with_strategie
    (strategie_play : game_update -> color -> move)
    (strategie_remove : game_update -> color -> coordinates) =
    { strategie_play; strategie_remove }

let privatePlay game_update (player1 : player_strategie) (private_player1 : player) (private_player2 : player) =
    let move = player1.strategie_play game_update private_player1.color in
    let newGU = apply game_update private_player1.color move in
    if not newGU.game_is_changed
    then raise (Not_Allowed "Illegal move")
    else if newGU.mill
    then
      let removed = player1.strategie_remove newGU private_player1.color in
      let newGU = eliminate_piece newGU removed private_player2.color in
      if not newGU.game_is_changed then raise (Not_Allowed "Illegal move") else newGU
    else newGU

let update_player_phase player =
    match player.phase with
    | Placing ->
        if player.piece_placed = max_pieces_per_player
        then
          {
            phase = Moving;
            color = player.color;
            piece_placed = player.piece_placed;
            nb_pieces_on_board = player.nb_pieces_on_board;
            bag = player.bag;
          }
        else player
    | Moving ->
        if player.nb_pieces_on_board = 3
        then
          {
            phase = Flying;
            color = player.color;
            piece_placed = player.piece_placed;
            nb_pieces_on_board = player.nb_pieces_on_board;
            bag = player.bag;
          }
        else player
    | Flying -> player

let update_phase game_update =
    {
      board = game_update.board;
      mill = game_update.mill;
      player1 = update_player_phase game_update.player1;
      player2 = update_player_phase game_update.player2;
      game_is_changed = game_update.game_is_changed;
    }

let arena (p1 : player_strategie) (p2 : player_strategie) =
    let private_p1 = init_player Black in
    let private_p2 = init_player White in
    let rec turn (game_update : game_update) =
        let game_update = update_phase game_update in
        if lost game_update private_p1
        then game_update
        else
          let newGU = privatePlay game_update p1 private_p1 private_p2 in
          if lost newGU private_p2
          then newGU
          else
            let game_update = update_phase game_update in
            let newGU = privatePlay game_update p2 private_p2 private_p1 in
            turn newGU
    in
    turn { board = init_board; mill = false; player1 = private_p1; player2 = private_p2; game_is_changed = false }
