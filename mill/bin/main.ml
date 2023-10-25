open Mill.Board

let b = initBoard
let () = prettyPrintBoard b

let (b,_) = placePiece b (0,0) Black
let (b,_) = placePiece b (3,0) White
let (b,_) = placePiece b (3,2) Black

let () = prettyPrintBoard b


let (b,_) = deplacer b (3,2) (4,2) Black
let () = prettyPrintBoard b

let (b,_) = deplacer b (4,2) (5,2) Black
let () = prettyPrintBoard b

;;print_endline "déplacement du pion (4,2) vers la droite";;
let (b,_) = moveToDirection b (4,2) Right Black
let () = prettyPrintBoard b

;;print_endline "déplacement du pion (4,3) vers le bas";;
let (b,_) = moveToDirection b (4,3) Down Black
let () = prettyPrintBoard b

;;print_endline "déplacement du pion (5,3) vers la droite"
let (b,_) = moveToDirection b (5,3) Right Black
let () = prettyPrintBoard b


;;print_endline "Test presence d'un moulin";;
let (b,_) = placePiece b (0,3) Black
let (b,_) = placePiece b (1,3) Black
let (b,rep) = placePiece b (2,3) Black

let printList (b : square list) : unit = List.iter (fun s -> printSquare s; Format.printf "@.") b ; print_endline ""
let () = prettyPrintBoard b
let () = if rep then print_endline "Moulin" else print_endline "No moulin"

let (b,rep) = placePiece b (4,3) Black
let () = prettyPrintBoard b
let () = if rep then print_endline "Moulin" else print_endline "No moulin"

let (b,_) = placePiece b (3,1) White
let (b,rep) = placePiece b (3,2) White
let () = prettyPrintBoard b
let () = if rep then print_endline "Moulin" else print_endline "No moulin"

;;print_endline "Test PossibleMove";;
let printMoveList (b : directionDeplacement list) : unit = List.iter (fun s -> printMove s; Format.printf "@.") b ; print_endline ""
let () = printMoveList( (possibleMoves b (3,2) White false))


;;print_endline "Test Column";;
let () = printList(getColumn b 3)