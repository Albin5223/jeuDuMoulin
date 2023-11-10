open Mill.Arena
open Mill.Player
open Mill.Type

let seed n =
    Random.self_init ();
    Random.int n

let p1 = player_randomly seed

let p2 = player_randomly seed

let game = arena p1 p2 Nine_mens_morris

let () = pretty_print_board game.board