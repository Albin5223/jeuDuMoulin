open Mill.Board;;
open Mill.Player;;
open Mill.Type;;

let current_phase = Placing

let initGame = {board= initBoard; mill = false; player1 = initPlayer Black; player2 = initPlayer White; gameIsChanged = false}

(*
let printBag player = 
  print_endline ("Bag : " ^ if player.color = Black then "Black" else "White");
  let rec aux = function
    | [] -> ()
    | (x,y)::q -> print_string ("(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"); aux q
  in aux player.bag ; print_endline ""
*)

let game = initGame

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

print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
print_int (Random.int board_size);
(*
let game = placeStartPiece game (0,0) Black
let () = prettyPrintBoard game.board
let game = placeStartPiece game (3,0) White
let () = prettyPrintBoard game.board
let game = placeStartPiece game (3,2) Black
let () = prettyPrintBoard game.board

let game = moveToDirection game (3,0) Left White
let () = prettyPrintBoard game.board
let () = printBag game.player1

let game = moveToDirection game (3,2) Left Black
let () = prettyPrintBoard game.board
let () = printBag game.player1

let game = placeStartPiece game (6,6) White
let () = prettyPrintBoard game.board
let () = printBag game.player2

let game = moveToCoordinates game (6,6) (0,6) White
let () = prettyPrintBoard game.board
let () = printBag game.player2

let game = placeStartPiece game (3,6) White
let game = placeStartPiece game (6,6) White
let () = prettyPrintBoard game.board

;;if game.mill then print_endline "Moulin" else print_endline "No moulin";;

let game = placeStartPiece game (6,3) White
let () = prettyPrintBoard game.board

;;if game.mill then print_endline "Moulin" else print_endline "No moulin";;
*)