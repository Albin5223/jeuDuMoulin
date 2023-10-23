open Board

(** The max amount of pieces that a player can hold *)
let max_pieces = 9

(** Represent a bag of pieces *)
type pieces = (color * coordonnee) list

(** Will represent the players *)
type player = {
  c : color;
  startPiecePlaced : int;
  remainingPieces : pieces;
  }