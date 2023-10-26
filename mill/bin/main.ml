open Mill.Board;;
open Mill.Player;;
open Mill.Type;;

(*let current_phase = Placing*)

let initGame = {board= initBoard2; mill = false; player1 = initPlayer Black; player2 = initPlayer White; gameIsChanged = false}


let printBag player = 
  print_endline ("Bag : " ^ if player.color = Black then "Black" else "White");
  let rec aux = function
    | [] -> ()
    | (x,y)::q -> print_string ("(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"); aux q
  in aux player.bag ; print_endline ""


let game = initGame

(*
let player1 = initPlayer White
let player2 = initPlayer Black

let game = playRandomly player1 player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let current_phase = Moving;;

print_endline "---------------------------------------------------------";;
print_endline "---------------------------------------------------------";;
print_endline "---------------------------------------------------------";;
print_endline "---------------------------------------------------------";;

let game = playRandomly game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board;
*)


let game = placeStartPiece game (0,0) Black
let () = prettyPrintBoard game.board
let () = printBag game.player1



let game = moveToDirection game (0,0) Right Black
let () = prettyPrintBoard game.board
let () = printBag game.player1

let game = moveToDirection game (0,6) Right Black
let () = prettyPrintBoard game.board
let () = printBag game.player1

let game = moveToDirection game (0,12) Down_left Black
let () = prettyPrintBoard game.board
let () = printBag game.player1

let game = moveToDirection game (2,10) Down_left Black
let () = prettyPrintBoard game.board
let () = printBag game.player1