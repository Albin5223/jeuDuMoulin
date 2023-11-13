type color = Black | White

type direction = H | V | DR | DL

type coordinates = int * int

type square = Empty | Path of direction | Wall | Color of color

type board = square list list

type phase = Placing | Moving | Flying

type direction_deplacement = Up | Down | Right | Left | Up_right | Up_left | Down_right | Down_left

type player = { phase: phase; color: color; piece_placed: int; nb_pieces_on_board: int; bag: coordinates list }

type move =
    | Placing of coordinates
    | Moving of coordinates * direction_deplacement
    | Flying of coordinates * coordinates

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

type got_mill = board * bool

type template = Three_mens_morris | Six_mens_morris | Nine_mens_morris | Twelve_mens_morris
