(** Defines the two type of piece, Black pieces and White pieces *)
type color = Black | White

(**
  Will be used in the type : square.
  Example : If a piece wants to move down, but the box below do not contain a "Path of V" (V for vertical), it means that it can't go down.   
*)
type direction = H | V | DR | DL

(** Coordinates *)
type coordinates = int*int

(** The grid squares, stored in our type board *)
type square =
  | Empty
  | Path of direction
  | Wall
  | Color of color

(** This will represent the game's board *)
type board = square list list

(** Will represent the players *)
type player = {
  color : color;
  piecePlaced : int;
  nbPiecesOnBoard : int;
  bag : coordinates list;
}

type phase =
  | Placing
  | Moving
  | Flying of player 
  | BothFlying

(** This type will be returned after each function that alterate the state of the game *)
type gameUpdate = {board : board; mill : bool; player1 : player; player2 : player; gameIsChanged : bool}

(** Will be returned after a move, and will let us know if the move produce a mill or not *)
type gotMill = board * bool

(** This type will be used when moving a piece to a certain direction *)
type directionDeplacement = 
  | Up
  | Down
  | Right
  | Left
  | Up_right
  | Up_left
  | Down_right
  | Down_left

