open Mill.Game;;
open Mill.Generator;;
open Mill.Board;;


let () = let game = gameRandomly randomAleaWithSeed in prettyPrintBoard game.board