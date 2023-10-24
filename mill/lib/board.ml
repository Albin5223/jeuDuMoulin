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

(** Will be returned after a move, and will let us know if the move was legit or not *)
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


let getSquare board (i,j) = 
  List.nth (List.nth board (i)) j

let getRow (board:board) (i:int) :square list =
  List.nth board i

let getColumn (board:board) (j:int) :square list=
  let rec aux board j acc =
    match board with 
    |[] -> acc
    |x::xs -> aux xs j [(List.nth x j)]@acc
  in aux board j []

let checkMillFromList subBoard (joueur:color) : int =
  List.fold_right (fun a b -> if a = Color(joueur) then b+1 else if a = Wall then 0 else b) subBoard 0
  

(**Function that check if there is a mill from a certain position(i,j)**)
let checkMillFromPosition (board:board) ((i,j):coordonnee) joueur:bool = 
  (checkMillFromList (getRow board i) joueur = 3) || 
  (checkMillFromList (getColumn board j) joueur = 3)

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

(*Rajouter une fonction qui en fonction d'une position renvoie la liste des mouvements possibles*)
(*Faire une IA qui joue au pif*)
(*Un bot qui repère une position qui empeche un moulin de l'adversaire*)
(*Faire le changement de phase entre il peut placer où il veut et ensuite deplacer de case en case*)
(*Faire des tests pour verifier les possibles erreurs*)

