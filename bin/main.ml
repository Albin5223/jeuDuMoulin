open Mill.Type
open Mill.Arena

let randomSeed n = Random.int n

let () =
    let a = arena player_human (player_random randomSeed) Twelve_mens_morris in
    pretty_print_board a.board
