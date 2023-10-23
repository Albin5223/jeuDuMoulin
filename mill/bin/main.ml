open Mill.Board

let b = initBoard
let () = prettyPrintBoard b

let b = positionner b (0,0) Black
let b = positionner b (3,0) White
let b = positionner b (3,2) Black

let () = prettyPrintBoard b


let b = deplacer b (3,2) (4,2) Black
let () = prettyPrintBoard b

let b = deplacer b (4,2) (5,2) Black
let () = prettyPrintBoard b
