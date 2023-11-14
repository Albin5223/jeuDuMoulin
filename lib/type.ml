(** Defines the two type of piece, Black pieces and White pieces *)
type color = Black | White

(**
  Will be used in the type : square.
  Example : If a piece wants to move down, but the box below do not contain a "Path of V" (V for vertical), it means that it can't go down.   
*)
type direction = H | V | DR | DL

(** Coordinates *)
type coordinates = int * int

(** The grid squares, stored in our type board *)
type square = Empty | Path of direction | Wall | Color of color

(** This will represent the game's board *)
type board = square list list

type phase = Placing | Moving | Flying

(** This type will be used when moving a piece to a certain direction *)
type direction_deplacement = Up | Down | Right | Left | Up_right | Up_left | Down_right | Down_left

(** Will represent the players *)
type player = { phase: phase; color: color; piece_placed: int; nb_pieces_on_board: int; bag: coordinates list }

(**This type represent an action*)
type move =
    | Placing of coordinates
    | Moving of coordinates * direction_deplacement
    | Flying of coordinates * coordinates

(** This type will be returned after each function that alterate the state of the game *)
type game_update = {
    board: board;
    mill: bool;
    player1: player;
    player2: player;
    game_is_changed: bool;
    max_pieces: int;
  }

type player_strategie = {
    strategie_play: game_update -> player -> move;
    strategie_remove: game_update -> player -> coordinates;
  }

(** Will be returned after a move, and will let us know if the move produce a mill or not *)
type got_mill = board * bool

(** Represent the name of defaults board (templates) *)
type template = Three_mens_morris | Six_mens_morris | Nine_mens_morris | Twelve_mens_morris

(**This type will be used after end game*)

type end_game = {
  board:board;
  winner: player;
  loser: player;
}
