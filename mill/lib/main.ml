type color = Black | White
type square =
  | Empty
  | Color of color

type player = {col : color; men : int}

let board = [[Color(Black);Color(White);Empty];[Empty;Color(White);Empty]];;

let printColor c = 
  match c with
  | White -> Format.printf "White@."
  | Black -> Format.printf "Black@."

let printSquare s = 
  match s with
  | Color(White) -> Format.printf "White,"
  | Color(Black) -> Format.printf "Black,"
  | Empty -> Format.printf "_,"

(*let printPlayer p = Format.printf "%d," p.men; printColor p.col

let printBoard (b : square list list) = List.iter (fun l -> List.iter (printSquare) l; Format.printf "@.") b

let player1 = {col=Black;men=9};;
printPlayer player1;;
printBoard board*)
