type color = Black | White
val pretty_print_color : color -> unit
type direction = H | V | DR | DL
type coordinates = int * int
type square = Empty | Path of direction | Wall | Color of color
type board = square list list
type phase = Placing | Moving | Flying
type direction_deplacement =
  |  Up
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
  piece_placed : int;
  nb_pieces_on_board : int;
  bag : coordinates list;
}
type move =
    Placing of coordinates
  | Moving of coordinates * direction_deplacement
  | Flying of coordinates * coordinates
  | Removing of coordinates
val get_removed_piece : move -> coordinates
type game_update = {
  board : board;
  mill : bool;
  player1 : player;
  player2 : player;
  game_is_changed : bool;
  max_pieces : int;
}
type player_strategie = {
  strategie_play : game_update -> player -> move;
  strategie_remove : game_update -> player -> move;
}
val get_player : game_update -> color -> player
type got_mill = board * bool
val pretty_print_phase : phase -> unit
val pretty_print_player : player -> unit
val reverse_color : color -> color
val get_opponent : game_update -> color -> player
val affiche_tour_info : color -> phase -> unit
val show_winner : color -> unit
val print_move : direction_deplacement -> Format.formatter -> unit
val pretty_print_move : move -> unit
val pretty_print_list_direction : direction_deplacement list -> unit
val print_cord : int * int -> unit
val print_square : square -> unit
val pretty_print_board : board -> unit
