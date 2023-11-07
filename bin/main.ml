open Mill.Type;;
open Mill.Board;;
(*open Mill.Generator;;
  open Mill.Game;;*)

let diagonal = false;;

let test = initBoard2 8 8 3 (diagonal) in
  prettyPrintBoard test

(*let () = let game = gameRandomly randomAlea in prettyPrintBoard game.board*)