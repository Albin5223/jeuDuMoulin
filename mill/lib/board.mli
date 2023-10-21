type color = Black | White
type direction = H | V
type coordonnee = int * int
type square = Empty | Path of direction | Wall | Color of color
val printSquare : square -> unit
val miseEnPlace : square list -> int -> color -> bool -> square list
val reconstitution : int -> square list -> square list list
val positionner : square list list -> coordonnee -> color -> square list list
val supprimer : 'a -> 'b -> 'c -> 'd list list
val deplacer :
  square list list -> coordonnee -> coordonnee -> color -> square list list
val prettyPrintBoard : square list list -> unit
val initBoard : square list list
