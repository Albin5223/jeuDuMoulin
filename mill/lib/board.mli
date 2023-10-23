val board_size : int
type color = Black | White
type direction = H | V
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
val boardMap : (square -> square) -> board -> coordonnee -> color -> board
val positionner : board -> int * int -> color -> board
val supprimer : board -> int * int -> color -> board
val deplacer : board -> coordonnee -> coordonnee -> color -> board
val prettyPrintBoard : board -> unit
val initBoard : square list list
