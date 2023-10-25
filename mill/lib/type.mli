type color = Black | White
type direction = H | V | DR | DL
type coordinates = int * int
type square = Empty | Path of direction | Wall | Color of color
type board = square list list
type player = {
  color : color;
  piecePlaced : int;
  remainingPieces : coordinates list;
}
type gameUpdate = {
  board : board;
  mill : bool;
  player1 : player;
  player2 : player;
  gameIsChanged : bool;
}
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
