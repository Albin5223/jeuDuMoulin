val board_size : int
type color = Black | White
type direction = H | V | DR | DL
type coordonnee = int * int
type square = Empty | Path of direction | Wall | Color of color
type board = square list list
type reponse = board * bool
type directionDeplacement =
    Up
  | Down
  | Right
  | Left
  | Up_right
  | Up_left
  | Down_right
  | Down_left
val printSquare : square -> unit
val boardMap : (square -> square) -> board -> coordonnee -> square list list
val placePiece : board -> int * int -> color -> board
val remove : board -> int * int -> color -> square list list
val deplacer : board -> coordonnee -> coordonnee -> color -> board
val prettyPrintBoard : board -> unit
val initBoard : square list list
val moveToDirection :
  board -> coordonnee -> directionDeplacement -> color -> board
