val max_pieces : int
type pieces = (Board.color * Board.coordonnee) list
type player = {
  c : Board.color;
  startPiecePlaced : int;
  remainingPieces : pieces;
}
