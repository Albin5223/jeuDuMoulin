open Mill.Type
open Mill.Arena
open Mill.Board
open Mill.Player

let equals_board (board1 : board) (board2 : board) : bool =
    let rec compare l1 l2 =
        match (l1, l2) with
        | [], [] -> true
        | [], _ -> false
        | _, [] -> false
        | x :: xs, y :: ys -> (
            match (x, y) with
            | Empty, Empty -> compare xs ys
            | Wall, Wall -> compare xs ys
            | Path d, Path g when d = g -> compare xs ys
            | Color c1, Color c2 when c1 = c2 -> compare xs ys
            | _ -> false)
    in
    compare (List.flatten board1) (List.flatten board2)

let equals_coordinate (c1 : coordinates) (c2 : coordinates) : bool = fst c1 = fst c2 && snd c1 = snd c2

let rec equals_list_coordinate (l1 : coordinates list) (l2 : coordinates list) : bool =
    match (l1, l2) with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | x :: xs, y :: ys -> equals_coordinate x y && equals_list_coordinate xs ys

let equals_player (p1 : player) (p2 : player) : bool =
    p1.color = p2.color
    && equals_list_coordinate p1.bag p2.bag
    && p1.piece_placed = p2.piece_placed
    && p1.nb_pieces_on_board = p2.nb_pieces_on_board

let equals_game_update (game1 : game_update) (game2 : game_update) : bool =
    equals_board game1.board game2.board
    && game1.mill = game2.mill
    && equals_player game1.player1 game2.player1
    && equals_player game1.player2 game2.player2
    && game1.game_is_changed = game2.game_is_changed

let test_config_end_game =
    let open QCheck in
    Test.make ~count:100 ~name:"for all game : one of players can't move or has two pieces left" small_int (fun _ ->
        let randomSeed n =
            Random.self_init ();
            Random.int n
        in
        let player1 = player_random randomSeed in
        let player2 = player_random randomSeed in
        let game = arena player1 player2 Nine_mens_morris in
        (cant_move game.player1 game || game.player1.nb_pieces_on_board <= 2)
        || cant_move game.player2 game
        || game.player2.nb_pieces_on_board <= 2)

(**This test check that with the same seed, we will get the same end*)
let testSeed =
    let open QCheck in
    Test.make ~count:10 ~name:"for all seed : END gamePlayWithSeed = END gamePlayWithSeed when both seeds are the same"
      small_int (fun x ->
        Random.init x;
        let randomSeed n = Random.int n in
        let player1 = player_random randomSeed in
        let player2 = player_random randomSeed in
        let game1 = arena player1 player2 Nine_mens_morris in
        Random.init x;
        let randomSeed n = Random.int n in
        let player1 = player_random randomSeed in
        let player2 = player_random randomSeed in
        let game2 = arena player1 player2 Nine_mens_morris in
        equals_game_update game1 game2)


let square_reachable_from_coordinates (i, j) (board : board) : board =
    let rec allReachable_from_coordinates (i, j) (board : board) (acc : direction_deplacement list) =
        let new_board, _ = place_piece_on_board board (i, j) Black in
        let rec loop board (i, j) list_of_direction =
            match list_of_direction with
            | [] -> board
            | x :: xs -> (
                let coord = node_from_direction board (i, j) x in
                match coord with
                | None -> loop board (i, j) xs
                | Some c -> (
                    let square = get_square board c in
                    match square with
                    | Some Empty -> let nv = allReachable_from_coordinates c board acc in loop nv (i,j) xs
                    | _ -> loop board (i, j) xs))
        in
        loop new_board (i, j) acc
    in
    allReachable_from_coordinates (i, j) board [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left]

let test_complete_board (board:board):bool =
    let rec aux i j =
        let size = List.length board in 
        if i = size && j = size 
            then true
        else 
            if j = size 
                then aux (i+1) 0
            else 
                let square = get_square board (i,j) in 
                match square with 
                |Some(Empty) -> false
                |_-> aux i (j+1)
            in aux 0 0

let generate_templates =
    let open QCheck in
    Gen.oneof [ Gen.return Six_mens_morris; Gen.return Three_mens_morris; Gen.return Nine_mens_morris]

let arbitrary_templates = QCheck.make generate_templates

let test_reachable = 
    let open QCheck in
    Test.make ~count:10000 ~name:"for all board : all square are reachable"
      (triple small_int small_int arbitrary_templates) 
      (fun (x,y,template) ->
        let board = init_template template in 
        let i = x mod (List.length board)in 
        let j = y mod (List.length board) in 
        let square = get_square board (i,j) in 
        match square with 
        |Some(Empty) -> let new_board = square_reachable_from_coordinates (i,j) board in test_complete_board new_board
        |_->true)

let () =
    let open Alcotest in
    run "SEED"
      [
        ("Test with Seed generate", [QCheck_alcotest.to_alcotest testSeed]);
        ("Test configuration end game", [QCheck_alcotest.to_alcotest test_config_end_game]);
        ("Test reachable square", [QCheck_alcotest.to_alcotest test_reachable]);
      ]
