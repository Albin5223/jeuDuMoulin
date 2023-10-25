(** This represent the size of the board **)
let board_size = 7

(** Defines the two type of piece, Black pieces and White pieces *)
type color = Black | White

(**
  Will be used in the type : square.
  Example : If a piece wants to move down, but the box below do not contain a "Path of V" (V for vertical), it means that it can't go down.   
*)
type direction = H | V | DR | DL

(** Coordinates *)
type coordonnee = int*int

(** The grid squares, stored in our type board *)
type square =
  | Empty
  | Path of direction
  | Wall
  | Color of color

(** This will represent the game's board *)
type board = square list list

(** Will be returned after a move, and will let us know if the move produce a mill or not *)
type reponse = board * bool

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

(** Function that print a board square *)
let printSquare (s : square) = 
  match s with
  | Color(White) -> Format.printf "{W}"
  | Color(Black) -> Format.printf "{B}"
  | Empty -> Format.printf "{ }"
  | Path(H) -> Format.printf "---"
  | Path(V) -> Format.printf " | "
  |_ -> Format.printf "   "

let printMove (m : directionDeplacement) = 
  match m with
  | Up -> Format.printf "Up"
  | Down -> Format.printf "Down"
  | Right -> Format.printf "Right"
  | Left -> Format.printf "Left"
  | Up_right -> Format.printf "Up_right"
  | Up_left -> Format.printf "Up_left"
  | Down_right -> Format.printf "Down_right"
  | Down_left -> Format.printf "Down_left"

let getSquare board (i,j) = 
  List.nth (List.nth board (i)) j

let getRow (board:board) (i:int) :square list =
  List.nth board i

let getColumn (board:board) (j:int) :square list=
  List.fold_right (fun l acc -> [(List.nth l j)] @acc) board []

let checkMillFromList subBoard (joueur:color) : int =
  List.fold_right (fun a b -> if a = Color(joueur) then b+1 else if a = Wall then 0 else b) subBoard 0

let checkMillInMid subBoard j joueur = 
  let rec aux subBoard distance count=
    match subBoard with 
    |[] -> count
    |Wall::_ when distance = 0-> count
    |x::xs when distance = 0 -> if x = Color(joueur) then aux xs distance (count+1) else aux xs distance count
    |_::xs -> aux xs (distance-1) count
  in if j<board_size/2 then aux subBoard 0 0 else aux subBoard 4 0


(**Function that check if there is a mill from a certain position(i,j)**)
  (*A FINIIIIR*)
let checkMillFromPosition (board:board) ((i,j):coordonnee) joueur:bool = 
  match i,j with 
  |(3,_) -> (checkMillFromList (getColumn board j) joueur = 3) || (checkMillInMid (getRow board i) j joueur = 3)
  |(_,3) -> (checkMillFromList (getRow board i) joueur = 3) || (checkMillInMid (getColumn board j) i joueur = 3)
  |_ ->(checkMillFromList (getRow board i) joueur = 3) || (checkMillFromList (getColumn board j) joueur = 3)

(** A map that apply the function "f" to the square at the coordinate (i,j) of the board *)
let boardMap (f:square -> square) (board:board) ((i,j):coordonnee) =
  List.mapi (fun x line -> if x = i then (List.mapi (fun y el -> if y = j then f el else el) line) else line) board

(**
  Function that put a piece on the board at the coordinate (i,j) and return the new board
  If the position is not legit for a piece, return the old board
*)
let placePiece board (i,j) joueur :reponse = 
  let board = boardMap (fun x -> if x = Empty then Color(joueur) else x) board (i,j) 
in let check = checkMillFromPosition board (i,j) joueur in (board,check)

(**
  This function remove a piece from the board and returns it
  Return an unchanged board if there is no piece in (i,j)
*)
let remove board (i,j) joueur =
  boardMap (fun x -> if x = Color(joueur) then Empty else x) board (i,j)

(**
  This function moves a piece from (i1,j1) to (i2,j2)
  Return the changed board if the move is legal, else, return the unchanged board
*)
let deplacer (board :board)((i1,j1):coordonnee) ((i2,j2):coordonnee) (joueur:color) :reponse = 
  let arrive = List.nth (List.nth board i2) j2 in
  let depart = List.nth (List.nth board i1) j1 in
  if arrive = Empty && depart = Color(joueur)
  then let sub = remove board (i1,j1) joueur in placePiece sub (i2,j2) joueur
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
let moveToDirection (b : board) ((i,j) : coordonnee) (d : directionDeplacement) (joueur:color) : reponse =
  let rec goTo (b : board) ((x,y) : coordonnee) (d : directionDeplacement) (joueur:color) : reponse =
    match d with
    | Up -> let case = List.nth (List.nth b (x-1)) y in if case = Path V then goTo b (x-1,y) d joueur else (if case = Empty then deplacer b (i,j) (x-1,y) joueur else (b,false))
    | Down -> let case = List.nth (List.nth b (x+1)) y in if case = Path V then goTo b (x+1,y) d joueur else (if case = Empty then deplacer b (i,j) (x+1,y) joueur else (b,false))
    | Right -> let case = List.nth (List.nth b x) (y+1) in if case = Path H then goTo b (x,y+1) d joueur else (if case = Empty then deplacer b (i,j) (x,y+1) joueur else (b,false))
    | Left -> let case = List.nth (List.nth b x) (y-1) in if case = Path H then goTo b (x,y-1) d joueur else (if case = Empty then deplacer b (i,j) (x,y-1) joueur else (b,false))
    | Up_right -> let case = List.nth (List.nth b (x-1)) (y+1) in if case = Path DR then goTo b (x-1,y+1) d joueur else (if case = Empty then deplacer b (i,j) (x-1,y+1) joueur else (b,false))
    | Up_left -> let case = List.nth (List.nth b (x-1)) (y-1) in if case = Path DL then goTo b (x-1,y-1) d joueur else (if case = Empty then deplacer b (i,j) (x-1,y-1) joueur else (b,false))
    | Down_right -> let case = List.nth (List.nth b (x+1)) (y+1) in if case = Path DL then goTo b (x+1,y+1) d joueur else (if case = Empty then deplacer b (i,j) (x+1,y+1) joueur else (b,false))
    | Down_left -> let case = List.nth (List.nth b (x+1)) (y-1) in if case = Path DR then goTo b (x+1,y-1) d joueur else (if case = Empty then deplacer b (i,j) (x+1,y-1) joueur else (b,false))
  in goTo b (i,j) d joueur


let possibleMoves (board : board) ((i,j) : coordonnee) (joueur : color) (diagonal : bool): directionDeplacement list = 
  let aux (board : board) ((i,j) : coordonnee) (dir : directionDeplacement) : directionDeplacement list =
    if i >= board_size || i < 0 || j >= board_size || j < 0 then []
    else if List.nth (List.nth board i) j = Empty then [dir] else []
  in
  match getSquare board (i,j) with
  | Color(p) when p = joueur ->
    let normalMoves = (aux board (i,j+1) Right)@(aux board (i,j-1) Left)@(aux board (i-1,j) Up)@(aux board (i+1,j) Down) in
    if diagonal then normalMoves@(aux board (i+1,j+1) Down_right)@(aux board (i-1,j+1) Up_right)@(aux board (i+1,j-1) Down_left)@(aux board (i-1,j-1) Up_left)
    else normalMoves
  | _ -> [] 


(*Faire une IA qui joue au pif*)
(*Faire le changement de phase entre il peut placer où il veut et ensuite deplacer de case en case*)
(*Faire des tests pour verifier les possibles erreurs*)

