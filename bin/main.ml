open Mill.Type
open Mill.Board

(*open Mill.Generator;;
  open Mill.Game;;*)

let diagonal = true;;

let test = initBoard2 24 24 6 diagonal in
prettyPrintBoard test

(*let () = let game = gameRandomly randomAlea in prettyPrintBoard game.board*)
