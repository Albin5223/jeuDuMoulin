open Mill.Engine
open Mill.Type

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

let equals_end_game (game1 : end_game) (game2 : end_game) : bool =
    equals_board game1.board game2.board
    && equals_player game1.loser game2.loser
    && equals_player game1.winner game2.winner

let game_update_of_game (game : end_game) : game_update =
    if game.winner.color = White
    then
      {
        board = game.board;
        mill = false;
        player1 = game.winner;
        player2 = game.loser;
        game_is_changed = false;
        max_pieces = game.winner.piece_placed;
      }
    else
      {
        board = game.board;
        mill = false;
        player1 = game.loser;
        player2 = game.winner;
        game_is_changed = false;
        max_pieces = game.winner.piece_placed;
      }

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
                    | Some Empty ->
                        let nv = allReachable_from_coordinates c board acc in
                        loop nv (i, j) xs
                    | _ -> loop board (i, j) xs))
        in
        loop new_board (i, j) acc
    in
    allReachable_from_coordinates (i, j) board [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left]

let board_map_all (f : square -> square) (board : board) : board =
    List.map (fun line -> List.map (fun el -> f el) line) board

let for_all_board (f : square -> bool) (board : board) : bool =
    List.for_all (fun line -> List.for_all (fun el -> f el) line) board

let fill_all_node (template : template) (color : color) : board =
    let b = init_board_with_template template in
    board_map_all
      (fun x ->
        match x with
        | Empty -> Color color
        | _ -> x)
      b

let test_complete_board (board : board) : bool =
    let rec aux i j =
        let size = List.length board in
        if i = size && j = size
        then true
        else if j = size
        then aux (i + 1) 0
        else
          let square = get_square board (i, j) in
          match square with
          | Some Empty -> false
          | _ -> aux i (j + 1)
    in
    aux 0 0

let generate_color =
    let open QCheck in
    Gen.oneof [Gen.return Black; Gen.return White]

let arbitrary_color =
    let open QCheck in
    make generate_color

let generate_templates =
    let open QCheck in
    Gen.oneof
      [
        Gen.return Six_mens_morris;
        Gen.return Three_mens_morris;
        Gen.return Nine_mens_morris;
        Gen.return Twelve_mens_morris;
      ]

let arbitrary_templates = QCheck.make generate_templates

let player_random_dumb (random : int -> int) : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            (* We also allow the bot to go outside the board by 1 square (to make him very dumb)*)
            let i = random (List.length game_update.board + 2) - 1 in
            let j = random (List.length game_update.board + 2) - 1 in
            Placing (i, j)
        | Moving ->
            let i = random (List.length player.bag + 2) - 1 in
            let coord = List.nth player.bag i in
            let possible_move = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
            let j = random (List.length possible_move + 2) - 1 in
            let dir = List.nth possible_move j in
            Moving (coord, dir)
        | Flying ->
            (* We also allow the bot to go outside the board by 1 square (to make him very dumb)*)
            let i = random (List.length game_update.board + 2) - 1 in
            let j = random (List.length game_update.board + 2) - 1 in
            let coord_arrive = (i, j) in
            let i = random (List.length player.bag) in
            let depart = List.nth player.bag i in
            Flying (depart, coord_arrive)
    in
    (* The removing strategy is here *)
    let strategie_remove (game_update : game_update) (player : player) : action =
        let i = random (List.length (get_opponent game_update player.color).bag) in
        Remove (List.nth (get_opponent game_update player.color).bag i)
    in
    { strategie_play; strategie_remove }

let player_invalid_pos : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            (* We also allow the bot to go outside the board by 1 square (to make him very dumb)*)
            let i = -1 in
            let j = -1 in
            Placing (i, j)
        | Moving ->
            let i = Random.int (List.length player.bag + 2) - 1 in
            let coord = List.nth player.bag i in
            let possible_move = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
            let j = Random.int (List.length possible_move + 2) - 1 in
            let dir = List.nth possible_move j in
            Moving (coord, dir)
        | Flying ->
            (* We also allow the bot to go outside the board by 1 square (to make him very dumb)*)
            let i = Random.int (List.length game_update.board + 2) - 1 in
            let j = Random.int (List.length game_update.board + 2) - 1 in
            let coord_arrive = (i, j) in
            let i = Random.int (List.length player.bag) in
            let depart = List.nth player.bag i in
            Flying (depart, coord_arrive)
    in
    (* The removing strategy is here *)
    let strategie_remove (game_update : game_update) (player : player) : action =
        let i = Random.int (List.length (get_opponent game_update player.color).bag) in
        Remove (List.nth (get_opponent game_update player.color).bag i)
    in
    { strategie_play; strategie_remove }

let generate_coordinates =
    let open QCheck in
    Gen.pair Gen.int Gen.int

let fill_template_with_colors (template : template) : board =
    let rec fill_row_template (row_template : square list) : square list =
        match row_template with
        | [] -> []
        | h :: t -> (
            match h with
            | Color _ -> h :: fill_row_template t
            | _ -> Color (QCheck.Gen.generate1 generate_color) :: fill_row_template t)
    in
    List.map fill_row_template (init_board_with_template template)

let triple_gen_template_coordinates_color =
    QCheck.Gen.(
      pair generate_templates (pair generate_coordinates generate_color)
      |> map (fun (template, (coordinates, color)) -> (template, coordinates, color)))

let arbitrary_triple_template_coordinates_color = QCheck.make triple_gen_template_coordinates_color
