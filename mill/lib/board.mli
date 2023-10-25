val board_size : int
val current_phase : int
val printSquare : Type.square -> unit
val printMove : Type.directionDeplacement -> unit
val getSquare : 'a list list -> int * int -> 'a
val getRow : Type.board -> int -> Type.square list
val getColumn : Type.board -> int -> Type.square list
val checkMillFromList : Type.square list -> Type.color -> int
val checkMillInMid : Type.square list -> int -> Type.color -> int
val checkMillFromPosition :
  Type.board -> Type.coordinates -> Type.color -> bool
val boardMap :
  (Type.square -> Type.square) ->
  Type.board -> Type.coordinates -> Type.square list list
val placePiece :
  Type.board -> Type.coordinates -> Type.player -> Type.gameUpdate
val remove : Type.board -> int * int -> Type.color -> Type.board
val moveToCoordinates :
  Type.board ->
  Type.coordinates -> Type.coordinates -> Type.player -> Type.gameUpdate
val prettyPrintBoard : Type.board -> unit
val initBoard : Type.square list list
val possibleMoves :
  Type.board ->
  Type.coordinates -> Type.color -> bool -> Type.directionDeplacement list
