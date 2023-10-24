open Mill.Board

let b = initBoard
let () = prettyPrintBoard b

let b = placePiece b (0,0) Black
let b = placePiece b (3,0) White
let b = placePiece b (3,2) Black

let () = prettyPrintBoard b


let b = deplacer b (3,2) (4,2) Black
let () = prettyPrintBoard b

let b = deplacer b (4,2) (5,2) Black
let () = prettyPrintBoard b

;;print_endline "déplacement du pion (4,2) vers la droite";;
let b = moveToDirection b (4,2) Right Black
let () = prettyPrintBoard b

;;print_endline "déplacement du pion (4,3) vers le bas";;
let b = moveToDirection b (4,3) Down Black
let () = prettyPrintBoard b

;;print_endline "déplacement du pion (5,3) vers la droite";;
let b = moveToDirection b (5,3) Right Black
let () = prettyPrintBoard b