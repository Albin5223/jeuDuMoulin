(** Defines the two type of piece, Black pieces and White pieces *)
type color = Black | White

let pretty_print_color (color : color) : unit =
    match color with
    | Black -> Format.printf "Black"
    | White -> Format.printf "White"

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
type player = {phase: phase; color: color; piece_placed: int; nb_pieces_on_board: int; bag: coordinates list}

(**This type represent an action*)
type move =
    | Placing of coordinates
    | Moving of coordinates * direction_deplacement
    | Flying of coordinates * coordinates
    | Removing of coordinates

let get_removed_piece (move : move) : coordinates =
    match move with
    | Removing (x, y) -> (x, y)
    | _ ->
        raise (Invalid_argument "Argument move is neither a Placing of coordinates neither a Removing of coordinates")

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
    strategie_remove: game_update -> player -> move;
  }

(*player1 is always White and player2 is always Black  *)

(**This function return a player who has the same color that the color in argument*)
let get_player (game_update : game_update) (color : color) : player =
    match color with
    | White -> game_update.player1
    | Black -> game_update.player2

(** Will be returned after a move, and will let us know if the move produce a mill or not *)
type got_mill = board * bool

let pretty_print_phase (p : phase) =
    match p with
    | Placing -> Format.printf "Phase de placement"
    | Moving -> Format.printf "Phase de deplacement"
    | Flying -> Format.printf "Phase de flying"

let pretty_print_player (player : player) : unit =
    Format.printf "{phase=";
    pretty_print_phase player.phase;
    Format.printf ";color=";
    pretty_print_color player.color;
    Format.printf ";piece_placed=%d;nb_pieces_on_board=%d}" player.piece_placed player.nb_pieces_on_board

let reverse_color (c : color) : color =
    match c with
    | Black -> White
    | White -> Black

let get_opponent game_update color = get_player game_update (reverse_color color)

let affiche_tour_info color phase =
    match color with
    | Black ->
        Format.printf "Le tour de BLACK\n";
        pretty_print_phase phase
    | White ->
        Format.printf "Le tour de  WHITE\n";
        pretty_print_phase phase

let show_winner color =
    match color with
    | Black -> Format.printf "Le vainqueur est BLACK\n"
    | White -> Format.printf "Le vainqueur est WHITE\n"

let print_move (m : direction_deplacement) (fmt : Format.formatter) =
    match m with
    | Up -> Format.fprintf fmt "Up@."
    | Down -> Format.fprintf fmt "Down@."
    | Right -> Format.fprintf fmt "Right@."
    | Left -> Format.fprintf fmt "Left@."
    | Up_right -> Format.fprintf fmt "Up_right@."
    | Up_left -> Format.fprintf fmt "Up_left@."
    | Down_right -> Format.fprintf fmt "Down_right@."
    | Down_left -> Format.fprintf fmt "Down_left@."

let pretty_print_move (move : move) : unit =
    match move with
    | Placing (x, y) -> Format.printf "Placing((%d,%d))@." x y
    | Moving ((x, y), dir) -> Format.printf "Moving((%d,%d))" x y ;(print_move dir Format.std_formatter);Format.printf "@."
    | Flying ((x, y), (x2, y2)) -> Format.printf "Flying((%d,%d),(%d,%d))" x y x2 y2
    | Removing (x, y) -> Format.printf "Removing((%d,%d))" x y

let pretty_print_list_direction l = l |> List.iter (fun a -> print_move a Format.std_formatter)

let print_cord (x, y) =
    let exit = "x :" ^ string_of_int x ^ " y :" ^ string_of_int y ^ "\n" in
    Format.printf "%s" exit

(** Function that print a board square *)
let print_square (s : square) =
    match s with
    | Color White -> Format.printf "{W}"
    | Color Black -> Format.printf "{B}"
    | Empty -> Format.printf "{ }"
    | Path H -> Format.printf "---"
    | Path V -> Format.printf " | "
    | Path DR -> Format.printf " / "
    | Path DL -> Format.printf " \\ "
    | _ -> Format.printf "   "

(** Print the board in the shell *)
let pretty_print_board (b : board) : unit =
    List.iter
      (fun l ->
        List.iter print_square l;
        Format.printf "@.")
      b;
    Format.printf "\n"
