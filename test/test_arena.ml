open Mill.Type
open Mill.Arena
open Mill.Engine
open Utils

(*TEST*)

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
        let game_update = game_update_of_game game in
        cant_move game.loser game_update || game.loser.nb_pieces_on_board <= 2)

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
        equals_end_game game1 game2)

let test_reachable =
    let open QCheck in
    Test.make ~count:10000 ~name:"for all board : all square are reachable"
      (triple small_int small_int arbitrary_templates) (fun (x, y, template) ->
        let board = init_board_with_template template in
        let i = x mod List.length board in
        let j = y mod List.length board in
        let square = get_square board (i, j) in
        match square with
        | Some Empty ->
            let new_board = square_reachable_from_coordinates (i, j) board in
            test_complete_board new_board
        | _ ->
            let new_board = square_reachable_from_coordinates (0, 0) board in
            test_complete_board new_board)

let test_error_player =
    let open QCheck in
    Test.make ~count:1000 ~name:"for all game : the dumb bot will never finish the game"
      (pair small_int arbitrary_templates) (fun (x, template) ->
        Random.init x;
        let randomSeed n = Random.int n in
        let player1 = player_random_dumb randomSeed in
        let player2 = player_random_dumb randomSeed in
        try
          let _ = arena player1 player2 template in
          false
        with
        | Not_Allowed _ | Invalid_Strategy _ -> true
        | _ -> false)

let () =
    let open Alcotest in
    run "TEST ARENA"
      [
        ("Test with Seed generate", [QCheck_alcotest.to_alcotest testSeed]);
        ("Test configuration end game", [QCheck_alcotest.to_alcotest test_config_end_game]);
        ("Test reachable square", [QCheck_alcotest.to_alcotest test_reachable]);
        ("Test error player", [QCheck_alcotest.to_alcotest test_error_player]);
      ]
