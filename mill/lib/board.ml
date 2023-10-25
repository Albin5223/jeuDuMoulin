(** This represent the size of the board **)
let board_size = 7
let current_phase = 1
(*
Phase 1 : Placing pieces
Phase 2 : Moving pieces
Phase 3 : "Flying"
*)

(** Defines the two type of piece, Black pieces and White pieces *)
type color = Black | White

let reverseColor (c : color) : color =
  match c with
  | Black -> White
  | White -> Black

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

type gameUpdate = {board : board; mill : bool; player1 : player; player2 : player; gameIsChanged : bool}

(** Will be returned after a move, and will let us know if the move was legit or not *)
type reponse = board * bool

(** This type will be used when moving a piece to a certain direction *)
type directionDeplacement = 
  | Up
  | Down
  | Right
  | Left
  | Diagonal of directionDeplacement * directionDeplacement

(** Function that print a board square *)
let printSquare (s : square) = 
  match s with
  | Color(White) -> Format.printf "{W}"
  | Color(Black) -> Format.printf "{B}"
  | Empty -> Format.printf "{ }"
  | Path(H) -> Format.printf "---"
  | Path(V) -> Format.printf " | "
  |_ -> Format.printf "   "


let getSquare board (i,j) = 
  List.nth (List.nth board (i)) j

let getRow (board:board) (i:int) :square list =
  List.nth board i

let getColumn (board:board) (j:int) :square list=
  let rec aux board j  =
    match board with 
    |[] -> []
    |x::xs -> [(List.nth x j)] @aux xs j 
  in aux board j

let checkMillFromList subBoard (player:color) : int =
  List.fold_right (fun a b -> if a = Color(player) then b+1 else if a = Wall then 0 else b) subBoard 0

let checkMillInMid subBoard j player = 
  let rec aux subBoard distance count=
    match subBoard with 
    |[] -> count
    |Wall::_ when distance = 0-> count
    |x::xs when distance = 0 -> if x = Color(player) then aux xs distance (count+1) else aux xs distance count
    |_::xs -> aux xs (distance-1) count
  in if j<board_size/2 then aux subBoard 0 0 else aux subBoard 4 0


(**Function that check if there is a mill from a certain position(i,j)**)
  (*A FINIIIIR*)
let checkMillFromPosition (board:board) ((i,j):coordinates) player:bool = 
  match i,j with 
  |(3,_) -> (checkMillFromList (getColumn board j) player = 3) || (checkMillInMid (getRow board i) j player = 3)
  |(_,3) -> (checkMillFromList (getRow board i) player = 3) || (checkMillInMid (getColumn board j) i player = 3)
  |_ ->(checkMillFromList (getRow board i) player = 3) || (checkMillFromList (getColumn board j) player = 3)

(** A map that apply the function "f" to the square at the coordinate (i,j) of the board *)
let boardMap (f:square -> square) (board:board) ((i,j):coordinates) =
  List.mapi (fun x line -> if x = i then (List.mapi (fun y el -> if y = j then f el else el) line) else line) board

(**
  Function that put a piece on the board at the coordinate (i,j) and return the new board
  If the position is not legit for a piece, return the old board
*)
let placePiece board (i,j) player : reponse = 
  let board = boardMap (fun x -> if x = Empty then Color(player) else x) board (i,j) 
in let check = checkMillFromPosition board (i,j) player in (board,check)

(**
  This function remove a piece from the board and returns it
  Return an unchanged board if there is no piece in (i,j)
*)
let remove board (i,j) player : board =
  boardMap (fun x -> if x = Color(player) then Empty else x) board (i,j)

(**
  This function moves a piece from (i1,j1) to (i2,j2)
  Return the changed board if the move is legal, else, return the unchanged board
*)
let move (board :board)((i1,j1):coordinates) ((i2,j2):coordinates) (player:color) :reponse = 
  let arrive = List.nth (List.nth board i2) j2 in
  let depart = List.nth (List.nth board i1) j1 in
  if arrive = Empty && depart = Color(player)
  then let sub = remove board (i1,j1) player in placePiece sub (i2,j2) player
  else (board,false)

(** Print the board in the shell *)
let prettyPrintBoard (b : board) : unit = (List.iter (fun l -> List.iter (printSquare) l; Format.printf "@.") b) ; print_endline ""

(** Init a start board *)
let initBoard =  
  [[Empty;Path(H);Path(H);Empty;Path(H);Path(H);Empty];
[Path(V);Empty;Path(H);Empty;Path(H);Empty;Path(V)];
[Path(V);Path(V);Empty;Empty;Empty;Path(V);Path(V)];
[Empty;Empty;Empty;Wall;Empty;Empty;Empty];
[Path(V);Path(V);Empty;Empty;Empty;Path(V);Path(V)];
[Path(V);Empty;Path(H);Empty;Path(H);Empty;Path(V)];
[Empty;Path(H);Path(H);Empty;Path(H);Path(H);Empty]]



(** Function that move a piece from the coordinate (i,j) to a certain direction *)
let moveToDirection (b : board) ((i,j) : coordinates) (d : directionDeplacement) (player:color) : reponse =
  let rec goTo (b : board) ((x,y) : coordinates) (d : directionDeplacement) (player:color) : reponse =
    match d with
    | Up -> let case = List.nth (List.nth b (x-1)) y in if case = Path V then goTo b (x-1,y) d player else (if case = Empty then deplacer b (i,j) (x-1,y) player else (b,false))
    | Down -> let case = List.nth (List.nth b (x+1)) y in if case = Path V then goTo b (x+1,y) d player else (if case = Empty then deplacer b (i,j) (x+1,y) player else (b,false))
    | Right -> let case = List.nth (List.nth b x) (y+1) in if case = Path H then goTo b (x,y+1) d player else (if case = Empty then deplacer b (i,j) (x,y+1) player else (b,false))
    | Left -> let case = List.nth (List.nth b x) (y-1) in if case = Path H then goTo b (x,y-1) d player else (if case = Empty then deplacer b (i,j) (x,y-1) player else (b,false))
    | Diagonal(Up,Right) -> let case = List.nth (List.nth b (x-1)) (y+1) in if case = Path DR then goTo b (x-1,y+1) d player else (if case = Empty then deplacer b (i,j) (x-1,y+1) player else (b,false))
    | Diagonal(Up,Left) -> let case = List.nth (List.nth b (x-1)) (y-1) in if case = Path DL then goTo b (x-1,y-1) d player else (if case = Empty then deplacer b (i,j) (x-1,y-1) player else (b,false))
    | Diagonal(Down,Right) -> let case = List.nth (List.nth b (x+1)) (y+1) in if case = Path DL then goTo b (x+1,y+1) d player else (if case = Empty then deplacer b (i,j) (x+1,y+1) player else (b,false))
    | Diagonal(Down,Left) -> let case = List.nth (List.nth b (x+1)) (y-1) in if case = Path DR then goTo b (x+1,y-1) d player else (if case = Empty then deplacer b (i,j) (x+1,y-1) player else (b,false))
    | _ -> board
  in goTo b (i,j) d player


let possibleMoves (board : board) ((i,j) : coordinates) (player : color) (diagonal : bool): directionDeplacement list = 
  let aux (board : board) ((i,j) : coordinates) (dir : directionDeplacement) : directionDeplacement list =
    if i >= board_size || i < 0 || j >= board_size || j < 0 then []
    else if List.nth (List.nth board i) j = Empty then [dir] else []
  in
  if getSquare board (i,j) != Color(player) then []
  else
    let normalMoves = (aux board (i+1,j) Right)@(aux board (i-1,j) Left)@(aux board (i,j-1) Up)@(aux board (i,j+1) Down) in
    if diagonal then normalMoves@(aux board (i+1,j+1) (Diagonal(Right,Down)))@(aux board (i+1,j-1) (Diagonal(Right,Up)))@(aux board (i-1,j+1) (Diagonal(Left,Down)))@(aux board (i-1,j-1) (Diagonal(Left,Up)))
    else normalMoves


(*Faire une IA qui joue au pif*)
(*Faire le changement de phase entre il peut placer où il veut et ensuite deplacer de case en case*)
(*Faire des tests pour verifier les possibles erreurs*)

