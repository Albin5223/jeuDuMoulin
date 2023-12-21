open Engine

let optimal_placing game_update =
    let free_positions = get_all_free_positions game_update in
    let index = Random.int (List.length free_positions) in
    let coord = List.nth free_positions index in
    Placing coord

let optimal_moving game_update player =
    let mill_movements =
        List.filter_map
          (fun (x, y) ->
            let moves_for_position = possible_moves_directions game_update (x, y) player.color in
            match List.length moves_for_position with
            | 0 -> None
            | _ -> (
                let available_coord =
                    List.map
                      (fun dir -> (dir, node_from_direction (get_board game_update) (x, y) dir))
                      moves_for_position
                in
                let move_with_mill =
                    List.filter
                      (function
                        | _, Some coord ->
                            let new_game_update = move_to_coordinates game_update (x, y) coord player.color in
                            check_mill_from_position (get_board new_game_update) coord player.color
                        | _ -> false)
                      available_coord
                in
                match List.length move_with_mill with
                | 0 -> None
                | _ ->
                    let random_index = Random.int (List.length move_with_mill) in
                    Some ((x, y), fst (List.nth move_with_mill random_index))))
          player.bag
    in
    match List.length mill_movements with
    | 0 ->
        let simple_movements =
            List.filter_map
              (fun (x, y) ->
                let moves_for_position = possible_moves_directions game_update (x, y) player.color in
                match List.length moves_for_position with
                | 0 -> None
                | _ -> (
                    let available_coord =
                        List.map
                          (fun dir -> (dir, node_from_direction (get_board game_update) (x, y) dir))
                          moves_for_position
                    in
                    match List.length available_coord with
                    | 0 -> None
                    | _ ->
                        let random_index = Random.int (List.length available_coord) in
                        Some ((x, y), fst (List.nth available_coord random_index))))
              player.bag
        in
        let coord, dir =
            let random_index = Random.int (List.length simple_movements) in
            List.nth simple_movements random_index
        in
        Moving (coord, dir)
    | _ ->
        let coord, dir =
            let random_index = Random.int (List.length mill_movements) in
            List.nth mill_movements random_index
        in
        Moving (coord, dir)

let find_mill_position game_update player free_positions =
    List.find_opt
      (fun coord ->
        let new_update = move_to_coordinates game_update (List.nth player.bag 0) coord player.color in
        check_mill_from_position (get_board new_update) coord player.color)
      free_positions

let optimal_flying game_update player =
    let pawn_to_move = List.nth player.bag (Random.int (List.length player.bag)) in
    let free_positions = get_all_free_positions game_update in
    let first_opposite_mill_position =
        find_mill_position game_update (get_opponent game_update player.color) free_positions
    in
    match first_opposite_mill_position with
    | Some pos -> Flying (pawn_to_move, pos)
    | None -> (
        let first_mill_position = find_mill_position game_update player free_positions in
        match first_mill_position with
        | Some pos -> Flying (pawn_to_move, pos)
        | None ->
            let random_free_position = List.nth free_positions (Random.int (List.length free_positions)) in
            Flying (pawn_to_move, random_free_position))

let player_optimal_Nathan =
    let strategie_play game_update player =
        match player.phase with
        | Placing -> optimal_placing game_update
        | Moving -> optimal_moving game_update player
        | Flying -> optimal_flying game_update player
    in
    let strategie_remove game_update player =
        let opposing_player = get_opponent game_update player.color in
        let to_remove_list =
            List.filter
              (fun coord -> check_mill_from_position (get_board game_update) coord opposing_player.color)
              opposing_player.bag
        in
        match List.length to_remove_list with
        | 0 -> Remove (List.nth opposing_player.bag 0)
        | _ -> Remove (List.nth to_remove_list 0)
    in

    { strategie_play; strategie_remove }
