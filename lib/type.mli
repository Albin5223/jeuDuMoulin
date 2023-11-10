type color = Black | White
type direction = H | V | DR | DL
type coordinates = int * int
type square = Empty | Path of direction | Wall | Color of color
type board = square list list
type phase = Placing | Moving | Flying
type directionDeplacement =
    Up
  | Down
  | Right
  | Left
  | Up_right
  | Up_left
  | Down_right
  | Down_left
type player = {
  phase : phase;
  color : color;
  piecePlaced : int;
  nbPiecesOnBoard : int;
  bag : coordinates list;
}
type move =
    Placing of coordinates
  | Moving of coordinates * directionDeplacement
  | Flying of coordinates * coordinates
type gameUpdate = {
  board : board;
  mill : bool;
  player1 : player;
  player2 : player;
  gameIsChanged : bool;
}
type playerStrategie = {
  stratPlay : gameUpdate -> move;
  stratRemove : gameUpdate -> coordinates;
}
val getPlayer : gameUpdate -> color -> player
type gotMill = board * bool
val prettyPrintPhase : phase -> unit
val reverseColor : color -> color
val getOpponent : gameUpdate -> color -> player
val afficheTourInfo : color -> phase -> unit
val afficheVainqueur : color -> unit
val printMove : directionDeplacement -> unit
val prettyPrintListDirection : directionDeplacement list -> unit
val printCord : int * int -> unit
val printSquare : square -> unit
val prettyPrintBoard : board -> unit
