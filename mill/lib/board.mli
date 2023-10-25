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
val printMove : directionDeplacement -> unit
val getSquare : 'a list list -> int * int -> 'a
val getRow : board -> int -> square list
val getColumn : board -> int -> square list
val checkMillFromList : square list -> color -> int
val checkMillInMid : square list -> int -> color -> int
val checkMillFromPosition : board -> coordonnee -> color -> bool
val boardMap : (square -> square) -> board -> coordonnee -> square list list
val placePiece : board -> int * int -> color -> reponse
val remove : board -> int * int -> color -> square list list
val deplacer : board -> coordonnee -> coordonnee -> color -> reponse
val prettyPrintBoard : board -> unit
val initBoard : square list list
val moveToDirection :
  board -> coordonnee -> directionDeplacement -> color -> reponse
val possibleMoves :
  board -> coordonnee -> color -> bool -> directionDeplacement list
