open Mill.Type
open Mill.Board

(*open Mill.Generator;;
  open Mill.Game;;*)

let diagonal = true;;

let test = init_board2 24 24 6 diagonal in
pretty_print_board test

(*let () = let game = gameRandomly randomAlea in prettyPrintBoard game.board*)
