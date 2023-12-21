open Engine

let random_position game_update =
    List.nth (get_all_free_positions game_update) (Random.int (List.length (get_all_free_positions game_update)))

let rec random_move random player game_update =
    let i = random (List.length player.bag) in
    let coord = List.nth player.bag i in
    let possible_move = possible_moves_directions game_update coord player.color in
    if List.length possible_move = 0
    then random_move random player game_update
    else
      let j = random (List.length possible_move) in
      let dir = List.nth possible_move j in
      (coord, dir)

let is_possible_move board pos dir =
    let final_pos = node_from_direction board pos dir in
    match final_pos with
    | None -> false
    | Some p -> get_square board p = Some Empty

let is_empty_pos board pos = get_square board pos = Some Empty

let moving_coords random game_update player =
    let board = get_board game_update in
    let dirs_avbl = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
    let avbl_moves pos dirs_avbl =
        (*dirs avbl for one pos*)
        List.fold_left (fun acc el -> if is_possible_move board pos el then acc @ [(pos, el)] else acc) [] dirs_avbl
    in
    let avbl_bag_moves =
        (*dirs for all pos of bag*)
        List.fold_left (fun acc el -> acc @ avbl_moves el dirs_avbl) [] player.bag
    in
    let possible_mill_pos = [(Up, Down); (Left, Right); (Up_right, Down_left); (Up_left, Down_right)] in
    (*find if one piece moved is adjacent to two opposite_color pieces*)
    let rec find_move_and_dir l =
        match l with
        | [] -> random_move random player game_update
        | x :: xs ->
            let rec check_possible_mill ext =
                match ext with
                | [] -> find_move_and_dir xs
                | y :: ys -> (
                    let future_x = node_from_direction board (fst x) (snd x) in
                    let extract_some a =
                        match a with
                        | None -> failwith ""
                        | Some k -> k
                    in
                    let first = node_from_direction board (extract_some future_x) (fst y) in
                    let second = node_from_direction board (extract_some future_x) (snd y) in
                    match (first, second) with
                    | None, None | None, _ | _, None -> check_possible_mill ys
                    | Some k, Some l ->
                        if get_square board k = Some (Color (reverse_color player.color))
                           && get_square board l = Some (Color (reverse_color player.color))
                        then x
                        else check_possible_mill ys)
            in
            check_possible_mill possible_mill_pos
    in

    find_move_and_dir avbl_bag_moves

let random_fly random game_update player =
    let rec choose_coord () =
        let i = random (board_length (get_board game_update)) in
        let j = random (board_length (get_board game_update)) in
        match get_square (get_board game_update) (i, j) with
        | Some Empty -> (i, j)
        | _ -> choose_coord ()
    in
    let coord_arrive = choose_coord () in
    let i = random (List.length player.bag) in
    let depart = List.nth player.bag i in
    (depart, coord_arrive)

let flying_phase random board game_update player =
    let random_piece =
        let i = random (List.length player.bag) in
        List.nth player.bag i
    in
    let length = board_length board - 1 in
    let enemy = get_opponent game_update player.color in
    let possible_row_of_pos (pos : coordinates) (bag : coordinates list) =
        List.find (fun p -> pos <> p && fst pos = fst p) bag
    in
    let rec possible_row bag =
        match bag with
        | [] -> None
        | x :: xs -> ( try Some (possible_row_of_pos x bag) with _ -> possible_row xs)
    in
    let possible_col_of_pos pos bag = List.find (fun p -> pos <> p && snd pos = snd p) bag in
    let rec possible_col bag =
        match bag with
        | [] -> None
        | x :: xs -> ( try Some (possible_col_of_pos x bag) with _ -> possible_col xs)
    in
    let psbl_c = possible_col enemy.bag in
    let psbl_r = possible_row enemy.bag in
    let find_missing_pos_row pos1 =
        let i = fst pos1 in
        let j = [0; length / 2; length] in
        let all_pos_of_i = List.map (fun jb -> (i, jb)) j in
        try Some (List.find (fun pos -> (not @@ List.mem pos enemy.bag) && is_empty_pos board pos) all_pos_of_i)
        with _ -> None
    in
    let find_missing_pos_col pos1 =
        let j = snd pos1 in
        let i = [0; length / 2; length] in
        let all_pos_of_j = List.map (fun ib -> (ib, j)) i in
        try Some (List.find (fun pos -> (not @@ List.mem pos enemy.bag) && is_empty_pos board pos) all_pos_of_j)
        with _ -> None
    in
    let random_fly = random_fly random game_update player in
    match psbl_r with
    | Some pos -> (
        match find_missing_pos_row pos with
        | Some p -> (random_piece, p)
        | None -> random_fly)
    | None -> (
        match psbl_c with
        | None -> random_fly
        | Some pos -> (
            match find_missing_pos_col pos with
            | Some p -> (random_piece, p)
            | None -> random_fly))

let my_rmv_strat random game_update player =
    let random_remove =
        let i = random (List.length (get_opponent game_update player.color).bag) in
        List.nth (get_opponent game_update player.color).bag i
    in
    let enemy = get_opponent game_update player.color in
    let possible_row_of_pos (pos : coordinates) (bag : coordinates list) =
        List.find (fun p -> pos <> p && fst pos = fst p) bag
    in
    let rec possible_row bag =
        match bag with
        | [] -> None
        | x :: xs -> ( try Some (possible_row_of_pos x bag) with _ -> possible_row xs)
    in
    let possible_col_of_pos pos bag = List.find (fun p -> pos <> p && snd pos = snd p) bag in
    let rec possible_col bag =
        match bag with
        | [] -> None
        | x :: xs -> ( try Some (possible_col_of_pos x bag) with _ -> possible_col xs)
    in
    let psbl_c = possible_col enemy.bag in
    let psbl_r = possible_row enemy.bag in
    let enemy_piece_found_row pos1 =
        let i = fst pos1 in
        try Some (List.find (fun pos -> fst pos = i) enemy.bag) with _ -> None
    in
    let enemy_piece_found_col pos1 =
        let j = snd pos1 in
        try Some (List.find (fun pos -> snd pos = j) enemy.bag) with _ -> None
    in
    match psbl_r with
    | Some pos -> (
        match enemy_piece_found_row pos with
        | Some p -> p
        | None -> random_remove)
    | None -> (
        match psbl_c with
        | None -> random_remove
        | Some pos -> (
            match enemy_piece_found_col pos with
            | Some p -> p
            | None -> random_remove))

let random = Random.int

let my_bot =
    let my_strat game_update player : action =
        let random_position =
            List.nth (get_all_free_positions game_update)
              (Random.int (List.length (get_all_free_positions game_update)))
        in
        let board = get_board game_update in
        let placing_action =
            if player.nb_pieces_on_board <= 1
            then
              (*Place in a corner first*)
              let length = board_length board - 1 in
              let corners = [(0, 0); (0, length); (length, 0); (length, length)] in
              let corner_is_avbl pos = List.exists (fun p -> p = pos) (get_all_free_positions game_update) in
              let avbl_corners = List.filter corner_is_avbl corners in
              let rec place_in_corner corners =
                  match corners with
                  | [] -> Placing random_position
                  | (x, y) :: rest -> (
                      match get_square board (x, y) with
                      | Some Empty -> Placing (x, y) (* Place if empty *)
                      | _ -> place_in_corner rest (* Place in the next corner *))
              in
              place_in_corner avbl_corners
            else
              let coords =
                  try
                    let board = get_board game_update in
                    let length = board_length (get_board game_update) - 1 in
                    if get_square board (0, length / 2) = Some Empty
                    then (0, length / 2)
                    else if get_square board (length / 2, 0) = Some Empty
                    then (length / 2, 0)
                    else if get_square board (length, length / 2) = Some Empty
                    then (length, length / 2)
                    else if get_square board (length / 2, length) = Some Empty
                    then (length / 2, length)
                    else random_position
                  with Not_found -> random_position
              in
              Placing coords
        in
        match player.phase with
        | Placing -> placing_action
        | Moving ->
            let move = moving_coords random game_update player in
            Moving (fst move, snd move)
        | Flying ->
            let move = flying_phase random board game_update player in
            Flying (fst move, snd move)
    in
    let rmv_strat game_update player =
        let piece_rmvd = my_rmv_strat random game_update player in
        Remove piece_rmvd
    in
    { strategie_play = my_strat; strategie_remove = rmv_strat }
