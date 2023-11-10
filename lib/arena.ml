open Type
open Board
open Player

exception Not_Allowed of string

let init_player (stratPlay : gameUpdate -> move) (stratRemove : gameUpdate -> coordinates) = { stratPlay; stratRemove }

let privatePlay gameUpdate (player1 : playerStrategie) (private_player1 : player) (private_player2 : player) =
    let move = player1.stratPlay gameUpdate in
    let newGU = apply gameUpdate private_player1.color move in
    if not newGU.gameIsChanged
    then raise (Not_Allowed "Illegal move")
    else if newGU.mill
    then
      let removed = player1.stratRemove newGU in
      let newGU = eliminatePiece newGU removed private_player2.color in
      if not newGU.gameIsChanged then raise (Not_Allowed "Illegal move") else newGU
    else newGU

let update_player_phase player =
    match player.phase with
    | Placing ->
        if player.piecePlaced = maxPiecesPerPlayer
        then
          {
            phase = Moving;
            color = player.color;
            piecePlaced = player.piecePlaced;
            nbPiecesOnBoard = player.nbPiecesOnBoard;
            bag = player.bag;
          }
        else player
    | Moving ->
        if player.nbPiecesOnBoard = 3
        then
          {
            phase = Flying;
            color = player.color;
            piecePlaced = player.piecePlaced;
            nbPiecesOnBoard = player.nbPiecesOnBoard;
            bag = player.bag;
          }
        else player
    | Flying -> player

let update_phase gameUpdate =
    {
      board = gameUpdate.board;
      mill = gameUpdate.mill;
      player1 = update_player_phase gameUpdate.player1;
      player2 = update_player_phase gameUpdate.player2;
      gameIsChanged = gameUpdate.gameIsChanged;
    }

let arena (p1 : playerStrategie) (p2 : playerStrategie) =
    let private_p1 = initPlayer Black in
    let private_p2 = initPlayer White in
    let rec turn (gameUpdate : gameUpdate) =
        let gameUpdate = update_phase gameUpdate in
        if lost gameUpdate private_p1
        then gameUpdate
        else
          let newGU = privatePlay gameUpdate p1 private_p1 private_p2 in
          if lost newGU private_p2
          then newGU
          else
            let gameUpdate = update_phase gameUpdate in
            let newGU = privatePlay gameUpdate p2 private_p2 private_p1 in
            turn newGU
    in
    turn { board = initBoard; mill = false; player1 = private_p1; player2 = private_p2; gameIsChanged = false }
