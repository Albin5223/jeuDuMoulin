type color = Black | White

type direction = H | V

type coordonnee = int*int

type square =
  | Empty
  | Path of direction
  | Wall
  | Color of color

let printSquare s = 
  match s with
  | Color(White) -> Format.printf "{W}"
  | Color(Black) -> Format.printf "{B}"
  | Empty -> Format.printf "{ }"
  | Path(H) -> Format.printf "---"
  | Path(V) -> Format.printf " | "
  |_ -> Format.printf "  "


let rec miseEnPlace liste i couleur trouve=
  match liste with 
  |[] -> []
  |x::xl -> if i = 0 && x = Empty then [Color(couleur)]@miseEnPlace xl (i-1) couleur true else [x]@miseEnPlace xl (i-1) couleur trouve


let reconstitution max (list:square list) :square list list =
  let rec aux max list i acc sublist =
    match list with
    |[] -> (acc@[sublist])
    |x::xl -> if i = max
              then aux max xl 1 (acc@[sublist]) [x]
              else aux max xl (i+1) acc (sublist@[x])
    in aux max list 0 [] []
  
let positionner (board :square list list)((i,j):coordonnee)(joueur:color) :square list list = 
  let subBoard = miseEnPlace (List.flatten board) (i*7+j) joueur false in
  
  reconstitution 7 subBoard 

let supprimer list i joueur= [[]]

(*Utilises le type option pour verifier que tout fonctionne bien*)
let deplacer (board :square list list)((i1,j1):coordonnee) ((i2,j2):coordonnee) (joueur:color) :square list list = 
  let subBoard = supprimer (List.flatten board) (i1*7+j1) joueur in positionner subBoard (i2,j2) joueur 



let prettyPrintBoard (b : square list list) = List.iter (fun l -> List.iter (printSquare) l; Format.printf "@.") b
let initBoard =  
  [[Empty;Path(H);Path(H);Empty;Path(H);Path(H);Empty];
[Path(V);Empty;Path(H);Empty;Path(H);Empty;Path(V)];
[Path(V);Path(V);Empty;Empty;Empty;Path(V);Path(V)];
[Empty;Empty;Empty;Wall;Empty;Empty;Empty];
[Path(V);Path(V);Empty;Empty;Empty;Path(V);Path(V)];
[Path(V);Empty;Path(H);Empty;Path(H);Empty;Path(V)];
[Empty;Path(H);Path(H);Empty;Path(H);Path(H);Empty]]

let board = initBoard
let () = prettyPrintBoard board

let board = positionner board (0,3) White 
let board = positionner board (0,0) Black 
let () = prettyPrintBoard board





