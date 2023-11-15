open Mill.Type
open Mill.Engine

let generate_color =
    let open QCheck in
    Gen.oneof [Gen.return Black; Gen.return White]

let arbitrary_color =
    let open QCheck in
    make generate_color

let test_place_piece =
    let open QCheck in
    Test.make ~name:"place_piece" ~count:1000 (triple arbitrary_color small_int small_int) (fun (color, x, y) ->
        let i = x mod 9 in
        let j = y mod 9 in
        let coord = (i, j) in
        let board = init_board_with_template Nine_mens_morris in
        if get_square board (i, j) == Some Empty
        then
          let board, _ = place_piece_on_board board coord color in
          get_square board coord = Some (Color color)
        else true)
    
let () =
    let open Alcotest in
    run "TEST ENGINE" [("Test place piece", [QCheck_alcotest.to_alcotest test_place_piece])]
