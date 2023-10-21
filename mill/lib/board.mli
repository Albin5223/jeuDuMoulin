type color = Black | White
type direction = H | V
type coordonnee = int * int
type square = Empty | Path of direction | Wall | Color of color
type board = square list list
val printSquare : square -> unit
val miseEnPlace : square list -> int -> color -> square list * bool
val reconstitution : int -> square list -> board
val positionner : board -> coordonnee -> color -> board
val supprimer : square list -> int -> color -> square list
val deplacer : board -> coordonnee -> coordonnee -> color -> board
val prettyPrintBoard : board -> unit
val initBoard : square list list
