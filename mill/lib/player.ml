open Type;;
open Board;;


let initPlayer (c : Type.color) : player = {color = c; bag = []; piecePlaced = 0; nbPiecesOnBoard = 0}

(** The max amount of pieces that a player can hold *)
let max_pieces = 9

let reverseColor (c : Type.color) : Type.color =
  match c with
  | Type.Black -> Type.White
  | Type.White -> Type.Black

let rec playRandomly (seed : int) (player : player) (opponent : player) (game : gameUpdate) (current_phase : phase) : gameUpdate =
  Random.init seed;
  if current_phase = Placing then 
    let (i,j) = (Random.int board_size,Random.int board_size) in
    let tmp = placeStartPiece game (i,j) player.color in
    if tmp.mill
      then eliminatePiece game (List.nth (opponent.bag) (Random.int opponent.nbPiecesOnBoard)) opponent.color
    else if not tmp.gameIsChanged then playRandomly seed player opponent game current_phase (*if we choose coordinates where a piece is already here or a path or a wall*)
    else tmp

  else if current_phase = Moving || current_phase = Flying(opponent.color) then (*either the bot can just move or the opponent is flying but not the bot*)
    let (x,y) = (List.nth player.bag (Random.int player.nbPiecesOnBoard)) in
    let movesPossible = possibleMoves game (x,y) player.color false in
    if List.length movesPossible = 0 then playRandomly seed player opponent game current_phase (*if we have a unmovable piece, we examine another piece*)
    else
      let tmp = moveToDirection game (x,y) (List.nth movesPossible (Random.int (List.length movesPossible))) player.color in
      if tmp.mill 
        then eliminatePiece game (List.nth (opponent.bag) (Random.int opponent.nbPiecesOnBoard)) opponent.color
      else tmp

  else (*this means either the bot is flying or both players are flying*)
    let (i,j) = (Random.int board_size,Random.int board_size) in
    let tmp = moveToCoordinates game ((List.nth (player.bag) (Random.int player.nbPiecesOnBoard))) (i,j) player.color in
    if tmp.mill then
      eliminatePiece game (List.nth (opponent.bag) (Random.int opponent.nbPiecesOnBoard)) opponent.color
    else if not tmp.gameIsChanged then playRandomly seed player opponent game current_phase (*if we choose coordinates inside a wall or a path*)
    else tmp