open Type
open Board

let initPlayer (c : Type.color) : player =
    { phase = Placing; color = c; bag = []; piecePlaced = 0; nbPiecesOnBoard = 0 }

(** The max amount of pieces that a player can hold *)
let max_pieces = 9

let cantMove (player : player) (game : gameUpdate) : bool =
    let rec aux (player : player) (game : gameUpdate) (bag : coordinates list) : bool =
        match bag with
        | [] -> true
        | (x, y) :: xs -> List.length (possibleMoves game (x, y) player.color) = 0 && aux player game xs
    in
    aux player game player.bag

let rec playRandomly random (player : color) (game : gameUpdate) (current_phase : phase) : gameUpdate =
    if current_phase = Placing
    then
      let i = random board_size in
      let j = random board_size in
      (*print_string ("I , J -> "^string_of_int i ^"; "^string_of_int j^"\n");*)
      let tmp = placeStartPiece game (i, j) player in
      (*(if tmp.mill then print_string ("MILLLLL"^ string_of_int (getPlayer tmp player).nbPiecesOnBoard^(string_of_int (getOpponent tmp player).nbPiecesOnBoard) ^"\n") else print_string "No milll \n");*)
      if tmp.mill
      then
        let pieceCC = List.nth (getOpponent tmp player).bag (random (getOpponent tmp player).nbPiecesOnBoard) in
        let newgame = eliminatePiece tmp pieceCC (getOpponent tmp player).color in
        newgame
      else if not tmp.gameIsChanged
      then
        playRandomly random player game
          current_phase (*if we choose coordinates where a piece is already here or a path or a wall*)
      else tmp
    else if current_phase = Moving
    then
      (*either the bot can just move or the opponent is flying but not the bot*)
      let x, y = List.nth (getPlayer game player).bag (random (getPlayer game player).nbPiecesOnBoard) in
      let movesPossible = possibleMoves game (x, y) player false in
      if List.length movesPossible = 0
      then playRandomly random player game current_phase (*if we have a unmovable piece, we examine another piece*)
      else
        let tmp = moveToDirection game (x, y) (List.nth movesPossible (random (List.length movesPossible))) player in
        if tmp.mill
        then
          let pieceCC = List.nth (getOpponent tmp player).bag (random (getOpponent tmp player).nbPiecesOnBoard) in
          let newgame = eliminatePiece tmp pieceCC (getOpponent tmp player).color in
          newgame
        else tmp
    else
      (*this means either the bot is flying or both players are flying*)
      let i = random board_size in
      let j = random board_size in
      let tmp =
          moveToCoordinates game
            (List.nth (getPlayer game player).bag (random (getPlayer game player).nbPiecesOnBoard))
            (i, j) player
      in
      if tmp.mill
      then
        let pieceCC = List.nth (getOpponent tmp player).bag (random (getOpponent tmp player).nbPiecesOnBoard) in
        let newgame = eliminatePiece tmp pieceCC (getOpponent tmp player).color in
        newgame
      else if not tmp.gameIsChanged
      then
        playRandomly random player game
          current_phase (*if we choose coordinates where a piece is already here or a path or a wall*)
      else tmp

let lost (game : gameUpdate) (player : player) : bool =
    if player.nbPiecesOnBoard <= 2 && player.piecePlaced = 9
    then true
    else
      match player.phase with
      | Moving | Flying -> cantMove player game
      | _ -> false
