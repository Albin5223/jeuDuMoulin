open Type
open Board
open Player

exception Not_Allowed of string

(**
  Function that init a strategic player
  @param strategie_play the strategie to play for the player (placing and moving pieces)
  @param strategie_remove the strategie to remove an opponent piece (when a mill is formed)
*)
let init_player_with_strategie
    (strategie_play : game_update -> player -> move)
    (strategie_remove : game_update -> player -> coordinates) =
    { strategie_play; strategie_remove }

(**
  Private function that is useful to play a turn for one player
  @param game_update the game_update
  @param player1 the player that is playing
  @param private_player1 the private player that is playing
  @param private_player2 the private player that is not playing (the opponent)
*)
let privatePlay game_update (player1 : player_strategie) (private_player1 : player) (private_player2 : player) =
    let move = player1.strategie_play game_update private_player1 in
    let newGU = apply game_update private_player1 move in
    if not newGU.game_is_changed
    then raise (Not_Allowed "Illegal move placed/move")
    else if newGU.mill
    then
      let removed = player1.strategie_remove newGU private_player1 in
      let newGU = eliminate_piece newGU removed private_player2.color in
      if not newGU.game_is_changed then raise (Not_Allowed "Illegal move remove") else newGU
    else newGU

(**
  Private function which update the phase of a player if necessary
  @param player the player to update    
*)
let update_player_phase player =
    match player.phase with
    | Placing ->
        if player.piece_placed
           = max_pieces (* if the player has placed all of his pieces, he can start moving them *)
        then
          {
            phase = Moving;
            color = player.color;
            piece_placed = player.piece_placed;
            nb_pieces_on_board = player.nb_pieces_on_board;
            bag = player.bag;
          }
        else player (* else, no changes *)
    | Moving ->
        if player.nb_pieces_on_board = 3 (* if the player has only 3 pieces left, he can start flying them *)
        then
          {
            phase = Flying;
            color = player.color;
            piece_placed = player.piece_placed;
            nb_pieces_on_board = player.nb_pieces_on_board;
            bag = player.bag;
          }
        else player (* else, no changes *)
    | Flying -> player (* if the player is already flying, no changes *)

(**
  Private function that update the phase of both players in the game_update
  @param game_update the game_update to update    
*)
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
        if lost game_update game_update.player1
        then game_update
        else
          let newGU = privatePlay game_update p1 game_update.player1 game_update.player2 in
          let newGU = update_phase newGU in
          if lost newGU newGU.player2
          then newGU
          else
            let newGU = privatePlay newGU p2 newGU.player2 newGU.player1 in
            turn newGU
    in
    turn {board = init_board2 12 12 3 false; mill = false; player1 = private_p1; player2 = private_p2; game_is_changed = false }