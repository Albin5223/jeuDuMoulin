type color = Black | White

type direction = H | V

type coordonnee = int*int

type square =
  | Empty
  | Path of direction
  | Wall
  | Color of color

type board = square list list
type reponse = board * bool
type directionDeplacement = 
  |UP
  |Down
  |Right
  |Left

let printSquare s = 
  match s with
  | Color(White) -> Format.printf "{W}"
  | Color(Black) -> Format.printf "{B}"
  | Empty -> Format.printf "{ }"
  | Path(H) -> Format.printf "---"
  | Path(V) -> Format.printf " | "
  |_ -> Format.printf "  "


let miseEnPlace liste i couleur =
  let rec aux liste i couleur acc trouve = 
    match liste with 
    |[] -> (acc,trouve)
    |x::xl -> if i = 0 && x = Empty 
      then aux xl (i-1) couleur (acc@[Color(couleur)]) true 
      else aux xl (i-1) couleur (acc@[x]) trouve 
    in aux liste i couleur [] false


let reconstitution max (list:square list) :board =
  let rec aux max list i acc sublist =
    match list with
    |[] -> (acc@[sublist])
    |x::xl -> if i = max
              then aux max xl 1 (acc@[sublist]) [x]
              else aux max xl (i+1) acc (sublist@[x])
    in aux max list 0 [] []


let positionner (board :board)((i,j):coordonnee)(joueur:color) :board = 
  let (subBoard,find) = miseEnPlace (List.flatten board) (i*7+j) joueur in
  if find then reconstitution 7 subBoard else board

let rec supprimer liste i joueur =
  match liste with 
  |[] -> []
  |x::xl -> if i = 0 && x = Color(joueur) then [Empty]@supprimer xl (i-1) joueur else [x]@supprimer xl (i-1) joueur
 

(*Utilises le type option pour verifier que tout fonctionne bien*)
let deplacer (board :board)((i1,j1):coordonnee) ((i2,j2):coordonnee) (joueur:color) :board = 
  let subBoard = supprimer (List.flatten board) (i1*7+j1) joueur in 
  let (subBoard,find) = miseEnPlace subBoard (i2*7+j2) joueur  
  in if find then reconstitution 7 subBoard else board  



let prettyPrintBoard (b : board) = List.iter (fun l -> List.iter (printSquare) l; Format.printf "@.") b
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



