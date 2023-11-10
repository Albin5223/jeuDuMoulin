open Mill.Type
open Mill.Arena
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

(**This test check that with the same seed, we will get the same end*)
let testSeed =
    let open QCheck in
    Test.make ~count:10 ~name:"for all seed : END gamePlayWithSeed = END gamePlayWithSeed when both seeds are the same"
      small_int (fun x ->
        Random.init x;
        let randomSeed n = Random.int n in
        let player1 = player_randomly randomSeed in let player2 = player_randomly randomSeed in 
        let game1 = arena player1 player2
        in Random.init x ;
        let randomSeed n = Random.int n in 
        let player1 = player_randomly randomSeed in let player2 = player_randomly randomSeed in 
        let game2 = arena player1 player2 in equals_game_update game1 game2)

let () =
    let open Alcotest in
    run "SEED" [("Test with Seed generate", [QCheck_alcotest.to_alcotest testSeed])]
