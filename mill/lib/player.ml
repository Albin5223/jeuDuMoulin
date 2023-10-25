open Type
open Board

(** The max amount of pieces that a player can hold *)
let max_pieces = 9

let reverseColor (c : Type.color) : Type.color =
  match c with
  | Type.Black -> Type.White
  | Type.White -> Type.Black

let play (player : player) (opponent : player) (board : Type.board) (current_phase : int) : Type.board =
  let (i,j) = List.nth (player.remainingPieces) (Random.int player.piecePlaced) in
  if current_phase = 1 then 
    let tmp = placePiece board (i,j) player in
    if tmp.mill
      then remove board (List.nth (opponent.remainingPieces) (Random.int opponent.piecePlaced)) opponent.color
    else tmp.board
  
  else if current_phase = 2 || current_phase = 3 then 
    let movesPossible = possibleMoves board (i,j) player.color false in
    let tmp = moveToDirection board (i,j) (List.nth movesPossible (Random.int (List.length movesPossible))) player.color in
    if tmp.mill 
      then remove board (List.nth (opponent.remainingPieces) (Random.int opponent.piecePlaced)) opponent.color
    else tmp.board
  
