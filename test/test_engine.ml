open Mill.Type
open Mill.Engine
open Utils

let test_move_to_cordinates (game_up : game_update) (col : color) =
    let open QCheck in
    Test.make ~name:"move_to_coord" ~count:1000 (quad small_int small_int small_int small_int)
      (fun (cx1, cx2, cy1, cy2) ->
        let i1 = cx1 mod 9 in
        let j1 = cx2 mod 9 in
        let i2 = cy1 mod 9 in
        let j2 = cy2 mod 9 in
        let dep = get_square game_up.board (i1, j1) in
        let arr = get_square game_up.board (i2, j2) in
        if dep = Some (Color col) && arr = Some Empty
        then not (equals_board game_up.board (move_to_coordinates game_up (i1, j1) (i2, j2) col).board)
        else equals_board game_up.board (move_to_coordinates game_up (i1, j1) (i2, j2) col).board)

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

let test_mill =
    let open QCheck in
    Test.make ~name:"mill" ~count:1000 (pair arbitrary_color arbitrary_templates) (fun (color, template) ->
        let board = fill_all_node template color in
        let flat = List.flatten board in
        let rec loop list acc x =
            match list with
            | [] -> acc
            | y :: ys -> (
                match y with
                | Color c when c = color ->
                    let j = x mod List.length board in
                    let i = x / List.length board in
                    let coord = (i, j) in
                    loop ys (acc && check_mill_from_position board coord color) (x + 1)
                | _ -> loop ys acc (x + 1))
        in
        loop flat true 0)

let () =
    let open Alcotest in
    run "TEST ENGINE"
      [
        ( "Test move_to_cordinates",
          [
            (let game_up =
                 {
                   board = init_board_with_template Twelve_mens_morris;
                   mill = false;
                   player1 = init_player White;
                   player2 = init_player Black;
                   game_is_changed = false;
                   max_pieces = max_piece_from_template Twelve_mens_morris;
                 }
             in
             QCheck_alcotest.to_alcotest (test_move_to_cordinates game_up Black));
          ] );
        ("Test place piece", [QCheck_alcotest.to_alcotest test_place_piece]);
        ("Test mill", [QCheck_alcotest.to_alcotest test_mill]);
      ]
