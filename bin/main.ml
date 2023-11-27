open Mill.Arena
open Mill.Engine

let () = Random.init 42

let randomSeed n = Random.int n

let () =
    let a = arena (player_random randomSeed) (player_random randomSeed) Nine_mens_morris in
    pretty_print_board (get_board a.game)
