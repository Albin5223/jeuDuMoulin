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
  | Flying of color 
  | BothFlying

(** This type will be returned after each function that alterate the state of the game *)
type gameUpdate = {board : board; mill : bool; player1 : player; player2 : player; gameIsChanged : bool}
(*player1 is always Black and player2 is always White  *)

(**This function return a player who has the same color that the color in argument*)
let getPlayer (gameUpdate:gameUpdate)(color:color):player =
  match color with
  |Black -> gameUpdate.player1
  |White ->  gameUpdate.player2



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

let prettyPrintPhase p = 
  match p with 
  | Placing -> print_string "Phase de placement\n"
  | Moving -> print_string "Phase de deplacement\n"
  | Flying (_) -> print_string "Phase de placement\n"
  | BothFlying -> print_string "Phase de vol\n"


let reverseColor (c : color) : color =
  match c with
  |Black ->White
  |White -> Black
  
let getOpponent gameUpdate color =
  getPlayer gameUpdate (reverseColor color)


let afficheTourInfo color phase = 
  match color with 
  |Black -> print_endline "Le tour de BLACK"; prettyPrintPhase phase
  |White -> print_endline "Le tour de  WHITE";prettyPrintPhase phase

let afficheVainqueur color = 
  match color with 
  |Black -> print_endline "Le vainqueur est BLACK"
  |White -> print_endline "Le vainqueur est WHITE"

let printMove (m : directionDeplacement) = 
  match m with
  | Up -> print_string "Up\n"
  | Down -> print_string "Down\n"
  | Right -> print_string "Right\n"
  | Left -> print_string "Left\n"
  | Up_right -> print_string "Up_right\n"
  | Up_left -> print_string "Up_left\n"
  | Down_right -> print_string "Down_right\n"
  | Down_left -> print_string "Down_left\n"

let prettyPrintListDirection l =
  l|>List.iter(fun a -> printMove a )

let printCord (x,y) = 
  print_string ("x :"^(string_of_int x)^" y :"^(string_of_int y)^"\n")


(** Function that print a board square *)
let printSquare (s : square) = 
  match s with
  | Color(White) -> Format.printf "{W}"
  | Color(Black) -> Format.printf "{B}"
  | Empty -> Format.printf "{ }"
  | Path(H) -> Format.printf "---"
  | Path(V) -> Format.printf " | "
  | Path(DR) -> Format.printf " / "
  | Path(DL) -> Format.printf " \\ "
  | _ -> Format.printf "   "
  

(** Print the board in the shell *)
let prettyPrintBoard (b : board) : unit = (List.iter (fun l -> List.iter (printSquare) l; Format.printf "@.") b) ; print_endline ""
