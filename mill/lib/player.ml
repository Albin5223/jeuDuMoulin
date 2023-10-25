open Type;;
open Board;;

let initPlayer (c : Type.color) : player = {color = c; bag = []; piecePlaced = 0; nbPiecesOnBoard = 0}

(** The max amount of pieces that a player can hold *)
let max_pieces = 9

let reverseColor (c : Type.color) : Type.color =
  match c with
  | Type.Black -> Type.White
  | Type.White -> Type.Black



let rec playRandomly (player : player) (opponent : player) (game : gameUpdate) (current_phase : phase) : gameUpdate =
  Random.self_init ();
  if current_phase = Placing then 
    let i = Random.int board_size in
    let j =  Random.int board_size in
    let tmp = placeStartPiece game (i,j) player.color in
    if tmp.mill
      then eliminatePiece game (List.nth (opponent.bag) (Random.int opponent.nbPiecesOnBoard)) opponent.color
    else if not tmp.gameIsChanged then playRandomly player opponent game current_phase
    else tmp

  else
    let (x,y) = (List.nth player.bag (Random.int player.nbPiecesOnBoard)) in
    let movesPossible = possibleMoves game (x,y) player.color false in
    if List.length movesPossible = 0 then playRandomly player opponent game current_phase
    else
      let tmp = moveToDirection game (x,y) (List.nth movesPossible (Random.int (List.length movesPossible))) player.color in
      if tmp.mill 
        then eliminatePiece game (List.nth (opponent.bag) (Random.int opponent.nbPiecesOnBoard)) opponent.color
      else tmp