(** This represent the size of the board *)
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

(** A map that apply the function "f" to the square at the coordinate (i,j) of the board *)
let boardMap (f:square -> square) (board:board) ((i,j):coordonnee) =
  List.mapi (fun x line -> if x = i then (List.mapi (fun y el -> if y = j then f el else el) line) else line) board

(**
  Function that put a piece on the board at the coordinate (i,j) and return the new board
  If the position is not legit for a piece, return the old board
*)
let placePiece board (i,j) joueur :board = 
  boardMap (fun x -> if x = Empty then Color(joueur) else x) board (i,j)

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
let deplacer (board :board)((i1,j1):coordonnee) ((i2,j2):coordonnee) (joueur:color) :board = 
  let arrive = List.nth (List.nth board i2) j2 in
  let depart = List.nth (List.nth board i1) j1 in
  if arrive = Empty && depart = Color(joueur)
  then let sub = remove board (i1,j1) joueur in placePiece sub (i2,j2) joueur
  else board

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


(*Fonction qui permet de vérifier en fonction d'une position qu'il y a bien un moulin*)


(** Function that move a piece from the coordinate (i,j) to a certain direction *)
let moveToDirection (b : board) ((i,j) : coordonnee) (d : directionDeplacement) (joueur:color) : board =
  let rec goTo (b : board) ((x,y) : coordonnee) (d : directionDeplacement) (joueur:color) : board =
    match d with
    | Up -> let case = List.nth (List.nth b (x-1)) y in if case = Path V then goTo b (x-1,y) d joueur else (if case = Empty then deplacer b (i,j) (x-1,y) joueur else b)
    | Down -> let case = List.nth (List.nth b (x+1)) y in if case = Path V then goTo b (x+1,y) d joueur else (if case = Empty then deplacer b (i,j) (x+1,y) joueur else b)
    | Right -> let case = List.nth (List.nth b x) (y+1) in if case = Path H then goTo b (x,y+1) d joueur else (if case = Empty then deplacer b (i,j) (x,y+1) joueur else b)
    | Left -> let case = List.nth (List.nth b x) (y-1) in if case = Path H then goTo b (x,y-1) d joueur else (if case = Empty then deplacer b (i,j) (x,y-1) joueur else b)
    | Up_right -> let case = List.nth (List.nth b (x-1)) (y+1) in if case = Path DR then goTo b (x-1,y+1) d joueur else (if case = Empty then deplacer b (i,j) (x-1,y+1) joueur else b)
    | Up_left -> let case = List.nth (List.nth b (x-1)) (y-1) in if case = Path DL then goTo b (x-1,y-1) d joueur else (if case = Empty then deplacer b (i,j) (x-1,y-1) joueur else b)
    | Down_right -> let case = List.nth (List.nth b (x+1)) (y+1) in if case = Path DL then goTo b (x+1,y+1) d joueur else (if case = Empty then deplacer b (i,j) (x+1,y+1) joueur else b)
    | Down_left -> let case = List.nth (List.nth b (x+1)) (y-1) in if case = Path DR then goTo b (x+1,y-1) d joueur else (if case = Empty then deplacer b (i,j) (x+1,y-1) joueur else b)
  in goTo b (i,j) d joueur

(*Rajoouter une fonction qui en fonction d'une position renvoie la liste des mouvements possibles*)
(*Faire une IA qui joue au pif*)
(*Un bot qui repère une position qui empeche un moulin de l'adversaire*)
(*Faire le changement de phase entre il peut placer où il veut et ensuite deplacer de case en case*)
(*Faire des tests pour verifier les possibles erreurs*)



