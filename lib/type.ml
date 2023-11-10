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
type game_update = { board: board; mill: bool; player1: player; player2: player; game_is_changed: bool }

type player_strategie = {
    strategie_play: game_update -> color -> move;
    strategie_remove: game_update -> color -> coordinates;
  }

(*player1 is always Black and player2 is always White  *)

(**This function return a player who has the same color that the color in argument*)
let get_player (game_update : game_update) (color : color) : player =
    match color with
    | Black -> game_update.player1
    | White -> game_update.player2

(** Will be returned after a move, and will let us know if the move produce a mill or not *)
type got_mill = board * bool

let pretty_print_phase (p : phase) =
    match p with
    | Placing -> Format.printf "Phase de placement<v>"
    | Moving -> Format.printf "Phase de deplacement<v>"
    | Flying -> Format.printf "Phase de placement<v>"

let reverse_color (c : color) : color =
    match c with
    | Black -> White
    | White -> Black

let get_opponent game_update color = get_player game_update (reverse_color color)

let affiche_tour_info color phase =
    match color with
    | Black ->
        Format.printf "Le tour de BLACK<v>";
        pretty_print_phase phase
    | White ->
        Format.printf "Le tour de  WHITE<v>";
        pretty_print_phase phase

let affiche_vainqueur color =
    match color with
    | Black -> Format.printf "Le vainqueur est BLACK<v>"
    | White -> Format.printf "Le vainqueur est WHITE<v>"

let print_move (m : direction_deplacement) =
    match m with
    | Up -> Format.printf "Up<v>"
    | Down -> Format.printf "Down<v>"
    | Right -> Format.printf "Right<v>"
    | Left -> Format.printf "Left<v>"
    | Up_right -> Format.printf "Up_right<v>"
    | Up_left -> Format.printf "Up_left<v>"
    | Down_right -> Format.printf "Down_right<v>"
    | Down_left -> Format.printf "Down_left<v>"

let pretty_print_list_direction l = l |> List.iter (fun a -> print_move a)

let print_cord (x, y) =
    let exit = "x :" ^ string_of_int x ^ " y :" ^ string_of_int y ^ "<v>" in
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
    Format.printf "<v>"
