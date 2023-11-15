open Mill.Type
open Mill.Arena

let () =
    let a = arena player_human player_human Twelve_mens_morris in
    pretty_print_board a.board
