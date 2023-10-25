open Board

(** The max amount of pieces that a player can hold *)
let max_pieces = 9

(** Will represent the players *)
type player = {
  c : color;
  piecePlaced : int;
  remainingPieces : coordonnee list;
}

let play (player : player) (opponent : player) (board : board) (current_phase : int) : board =
  let (i,j) = List.nth (player.remainingPieces) (Random.int piecePlaced) in
  if current_phase = 1 then (
    let tmp = placePiece board (i,j) player.color in
    if tmp.mill then remove board (List.nth (opponent.remainingPieces) (Random.int opponent.piecePlaced)) opponent.color
    else tmp.board
  )
  if current_phase = 2 || current_phase = 3 then (
    let movesPossible = possibleMoves board (i,j) player.color false in
    let tmp = moveToDirection board (i,j) (List.nth movesPossible (Random.int (List.length movesPossible))) player.color in
    if tmp.mill then remove board (List.nth (opponent.remainingPieces) (Random.int opponent.piecePlaced)) opponent.color
    else tmp.board
  )
