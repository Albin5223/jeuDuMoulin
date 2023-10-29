open Mill.Game;;
open Mill.Generator;;
open Mill.Board;;

<<<<<<< bin/main.ml
let current_phase = Placing

let quarter =
  [
    [Empty; Path(H); Path(H); Path(H); Path(H); Path(H); Empty];
    [Path(V); Wall; Wall; Wall; Wall; Wall; Path(V)];
    [Path(V); Wall; Empty; Path(H); Path(H);Path(H);Empty];
    [Path(V); Wall; Path(V); Wall; Wall; Wall; Path(V)];
    [Path(V); Wall; Path(V); Wall; Empty; Path(H); Empty];
    [Path(V); Wall; Path(V); Wall; Path(V); Wall; Wall];
    [Empty; Path(H); Empty; Path(H); Empty; Wall; Wall]
  ]

let gameboard = initBoardQuarter quarter

let initGame = {board= gameboard; mill = false; player1 = initPlayer Black; player2 = initPlayer White; gameIsChanged = false}

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

let game = playRandomly randomAleaWithSeed player1 player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let current_phase = Moving;;

print_endline "---------------------------------------------------------";;
print_endline "---------------------------------------------------------";;
print_endline "---------------------------------------------------------";;
print_endline "---------------------------------------------------------";;

let game = playRandomly randomAleaWithSeed game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player1 game.player2 game current_phase 
let () = prettyPrintBoard game.board

let game = playRandomly randomAleaWithSeed game.player2 game.player1 game current_phase 
let () = prettyPrintBoard game.board;

(*
let game = placeStartPiece game (0,0) Black
let () = prettyPrintBoard game.board
let game = placeStartPiece game (3,0) White
let () = prettyPrintBoard game.board
let game = placeStartPiece game (3,2) Black
let () = prettyPrintBoard game.board


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

;;if game.mill then print_endline "Moulin" else print_endline "No moulin";;
*)
=======
>>>>>>> bin/main.ml

let () = let game = gameRandomly randomAleaWithSeed in prettyPrintBoard game.board