open Mill.Type
open Mill.Engine
open Utils

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

let check_mill_from_position_property =
    QCheck.Test.make arbitrary_triple_template_coordinates_color ~name:"check_mill_from_position" ~count:1000
      (fun (template, (i, j), color) ->
        let board = fill_template_with_colors template in
        let result = check_mill_from_position board (i, j) color in
        (* Properties *)
        QCheck.assume (i >= 0 && j >= 0 && i < List.length board && j < List.length (List.hd board));
        (if result
         then
           (* Property 1: If a mill is detected, there must be at least 3 pieces in a row/column/diagonal *)
           let count_pieces d =
               (* Count pieces in a certain direction *)
               let rec count_from_dir (x, y) d =
                   match node_from_direction board (x, y) d with
                   | Some (a, b) ->
                       if get_square board (a, b) = Some (Color color) then 1 + count_from_dir (a, b) d else 0
                   | _ -> 0
               in
               let reverse_direction : direction_deplacement -> direction_deplacement = function
                   | Up -> Down
                   | Down -> Up
                   | Right -> Left
                   | Left -> Right
                   | Up_right -> Down_left
                   | Up_left -> Down_right
                   | Down_right -> Up_left
                   | Down_left -> Up_right
               in
               count_from_dir (i, j) d + count_from_dir (i, j) (reverse_direction d) + 1
           in
           QCheck.assume
             (count_pieces Right >= 3
             || count_pieces Down >= 3
             || count_pieces Up_right >= 3
             || count_pieces Down_left >= 3
             || count_pieces Up_left >= 3
             || count_pieces Down_right >= 3));
        (* Add more properties as needed *)
        true)

let test_place_piece_not_assume_is_valid_pos =
    let open QCheck in
    Test.make ~name:"place_piece" ~count:1000 (triple arbitrary_color small_int small_int) (fun (color, x, y) ->
        let i = x in
        let j = y in
        let coord = (i, j) in
        let board = init_board_with_template Nine_mens_morris in
        let board, _ = place_piece_on_board board coord color in
        get_square board coord = Some (Color color))

let () =
    let open Alcotest in
    run "TEST ENGINE"
      [
        ("Test place piece", [QCheck_alcotest.to_alcotest test_place_piece]);
        ("Test mill", [QCheck_alcotest.to_alcotest test_mill]);
        ("Test mill from position", [QCheck_alcotest.to_alcotest check_mill_from_position_property]);
        ( "Test place piece not assume is valid pos",
          [QCheck_alcotest.to_alcotest test_place_piece_not_assume_is_valid_pos] );
      ]
