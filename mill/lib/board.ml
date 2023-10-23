let board_size = 7

(** Defines the two type of piece, Black pieces and White pieces *)
type color = Black | White

(**
  Will be used in the type : square.
  Example : If a piece wants to move down, but the box below do not contain a "Path of V" (V for vertical), it means that it can't go down.   
*)
type direction = H | V

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
  |_ -> Format.printf "  "



let boardMap (f:square -> square) (board :board)((i,j):coordonnee)(joueur:color) :board = 
  let rec parcoursRow board (i,j) joueur finalBoard =
    match (board,i) with
    |([],_) -> finalBoard
    |(x::xs,0) -> let rec parcoursLine (ligne:square list) (j:int) joueur=
                    match (ligne,j) with
                    |([],_) -> []
                    |(x::ys,0) -> let square = f x in [square]@parcoursLine ys (j-1) joueur
                    |(y::ys,b) -> [y]@parcoursLine ys (b-1) joueur
                  in parcoursRow xs (i-1,j) joueur (finalBoard@[(parcoursLine x j joueur)]) 
    |(x::xs,a) -> parcoursRow xs (a-1,j) joueur (finalBoard@[x])
  in parcoursRow board (i,j) joueur []

let positionner board (i,j) joueur :board = 
  boardMap (fun x -> if x = Empty then Color(joueur) else x) board (i,j) joueur


(** This function remove a piece from the board and returns it *)
let supprimer board (i,j) joueur =
  boardMap (fun x -> if x = Color(joueur) then Empty else x) board (i,j) joueur

(** This function moves a piece with a direction *)
let deplacer (board :board)((i1,j1):coordonnee) ((i2,j2):coordonnee) (joueur:color) :board = 
  let arrive = List.nth (List.nth board i2) j2 in
  let depart = List.nth (List.nth board i1) j1 in
  if arrive = Empty && depart =Color(joueur)
  then let sub = supprimer board (i1,j1) joueur in positionner sub (i2,j2) joueur
  else board



(** Print the board in the shell *)
let prettyPrintBoard (b : board) = List.iter (fun l -> List.iter (printSquare) l; Format.printf "@.") b

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
(*Rajouter une fonction qui déplace un pion d'une seule case en fonction de la direction donnée*)
(*Rajoouter une fonction qui en fonction d'une position renvoie la liste des mouvements possibles*)
(*Faire une IA qui joue au pif*)
(*Un bot qui repère une position qui empeche un moulin de l'adversaire*)
(*Faire le changement de phase entre il peut placer où il veut et ensuite deplacer de case en case*)
(*Faire des tests pour verifier les possibles erreurs*)



