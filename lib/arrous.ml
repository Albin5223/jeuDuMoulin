open Engine

let random = Random.int

let player_classic_bot_Thomas_Arrous : player_strategie =
    let can_make_a_mill_in_one_move_at_coord coord player game_update =
        let rec iter_on_side_case directions =
            match directions with
            | h_dir :: t_dir -> (
                match node_from_direction (get_board game_update) coord h_dir with
                | Some coord_to_move -> (
                    match get_square (get_board game_update) coord_to_move with
                    | Some (Color c) when c = player.color ->
                        let new_game_update = move_to_coordinates game_update coord_to_move coord player.color in
                        if check_mill_from_position (get_board new_game_update) coord player.color
                        then true
                        else iter_on_side_case t_dir
                    | _ -> false)
                | _ -> false)
            | [] -> true
        in
        iter_on_side_case [Up; Down; Left; Right; Up_right; Up_left; Down_right; Down_left]
    in
    let find_possibilities_of_mill_in_one_move
        (game_update : game_update)
        (player : player)
        (without_free_mill_for_my_enemy : bool) =
        let find_possibilities_of_mill_of_player
            (can_my_self_mill : (coordinates * direction_deplacement) list)
            (current_coordinates : coordinates) =
            let posible_move = possible_moves_directions game_update current_coordinates player.color in
            let rec iter_move
                (posible_move : direction_deplacement list)
                (can_my_self_mill : ((int * int) * direction_deplacement) list) =
                match posible_move with
                | h_posible_move :: t_posible_move -> (
                    let new_coord = node_from_direction (get_board game_update) current_coordinates h_posible_move in
                    match new_coord with
                    | None -> iter_move t_posible_move can_my_self_mill
                    | Some new_coord ->
                        let try_game_update =
                            move_to_coordinates game_update current_coordinates new_coord player.color
                        in
                        let opponent = get_opponent game_update player.color in
                        if without_free_mill_for_my_enemy
                           && can_make_a_mill_in_one_move_at_coord current_coordinates opponent try_game_update
                        then iter_move t_posible_move can_my_self_mill
                        else if check_mill_from_position (get_board try_game_update) new_coord player.color
                        then iter_move t_posible_move ((current_coordinates, h_posible_move) :: can_my_self_mill)
                        else iter_move t_posible_move can_my_self_mill)
                | [] -> can_my_self_mill
            in
            iter_move posible_move can_my_self_mill
        in
        List.fold_left (fun acc coord -> find_possibilities_of_mill_of_player acc coord) [] player.bag
    in
    let find_all_valid_moves game_update player =
        let find_valid_move
            (valid_move : (coordinates * direction_deplacement) list)
            (current_coordinates : coordinates) =
            let posible_move = possible_moves_directions game_update current_coordinates player.color in
            let rec iter_move
                (posible_move : direction_deplacement list)
                (valid_move : ((int * int) * direction_deplacement) list) =
                match posible_move with
                | h_posible_move :: t_posible_move ->
                    iter_move t_posible_move ((current_coordinates, h_posible_move) :: valid_move)
                | [] -> valid_move
            in
            iter_move posible_move valid_move
        in
        List.fold_left (fun acc coord -> find_valid_move acc coord) [] player.bag
    in
    let rec correspond_move_to_go_on (end_coord : coordinates) move_list (game_update : game_update) =
        match move_list with
        | (c, d) :: t ->
            if node_from_direction (get_board game_update) c d = Some end_coord
            then (c, d)
            else correspond_move_to_go_on end_coord t game_update
        | [] -> ((-1, -1), Up)
    in
    let intersection_between_two_lists list1 list2 =
        List.fold_left (fun acc elem -> if List.exists (fun x -> x = elem) list1 then elem :: acc else acc) [] list2
    in
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            (* When the bot is in Placing phase, he chooses a random square where to place, and repeat that until he finds a correct position *)
            let get_three_tuple (i : int) =
                match i with
                (*line*)
                | 0 -> ((0, 0), (0, 6), (0, 12))
                | 1 -> ((2, 2), (2, 6), (2, 10))
                | 2 -> ((4, 4), (4, 6), (4, 8))
                | 3 -> ((6, 0), (6, 2), (6, 4))
                | 4 -> ((6, 8), (6, 10), (6, 12))
                | 5 -> ((8, 4), (8, 6), (8, 8))
                | 6 -> ((10, 2), (10, 6), (10, 10))
                | 7 -> ((12, 0), (12, 6), (12, 12))
                (*column*)
                | 8 -> ((0, 0), (6, 0), (12, 0))
                | 9 -> ((2, 2), (6, 2), (10, 2))
                | 10 -> ((4, 4), (6, 4), (8, 4))
                | 11 -> ((0, 6), (2, 6), (4, 6))
                | 12 -> ((8, 6), (10, 6), (12, 6))
                | 13 -> ((4, 8), (6, 8), (8, 8))
                | 14 -> ((2, 10), (6, 10), (10, 10))
                | _ -> ((0, 12), (6, 12), (12, 12))
            in
            let filter_empty_coordonnee_on_three_tuple (i : int) (find_list : coordinates list) =
                let a, b, c = get_three_tuple i in
                let filter_empty_coordonnee (coordonnee : coordinates) (find_list : coordinates list) =
                    let info = get_square (get_board game_update) coordonnee in
                    match info with
                    | Some (Color _) -> find_list
                    | _ -> coordonnee :: find_list
                in
                filter_empty_coordonnee c (filter_empty_coordonnee b (filter_empty_coordonnee a find_list))
            in
            let get_number_of_each_piece_on_three_tuple (i : int) =
                let a, b, c = get_three_tuple i in
                let get_number_of_each_piece
                    (coordonnee : coordinates)
                    ((my_piece, enemy_piece, empty_place) : int * int * int) =
                    let info = get_square (get_board game_update) coordonnee in
                    match info with
                    | Some (Color c) when c = player.color -> (1 + my_piece, enemy_piece, empty_place)
                    | Some (Color c) when c <> player.color -> (my_piece, 1 + enemy_piece, empty_place)
                    | _ -> (my_piece, enemy_piece, 1 + empty_place)
                in
                get_number_of_each_piece c (get_number_of_each_piece b (get_number_of_each_piece a (0, 0, 0)))
            in
            let make_choice_with_priority_sort
                ((can_my_self_mill, can_block_enemy_mill, other_position) :
                  (int * int) list * (int * int) list * (int * int) list) =
                let best = intersection_between_two_lists can_my_self_mill can_block_enemy_mill in
                if List.length best <> 0
                then List.nth best (random (List.length best))
                else if List.length can_my_self_mill <> 0
                then List.nth can_my_self_mill (random (List.length can_my_self_mill))
                else if List.length can_block_enemy_mill <> 0
                then List.nth can_block_enemy_mill (random (List.length can_block_enemy_mill))
                else
                  let best_position =
                      List.filter
                        (fun (a, b) -> List.for_all (fun (x, y) -> a <> x && b <> y) player.bag)
                        other_position
                  in
                  match best_position with
                  | [] -> List.nth other_position (random (List.length other_position))
                  | _ -> List.nth best_position (random (List.length best_position))
            in
            let rec priority_sort_from_the_board
                (i : int)
                ((can_my_self_mill, can_block_enemy_mill, other_position) :
                  (int * int) list * (int * int) list * (int * int) list) =
                match i with
                | 16 -> make_choice_with_priority_sort (can_my_self_mill, can_block_enemy_mill, other_position)
                | _ ->
                    let my_piece, enemy_piece, empty_place = get_number_of_each_piece_on_three_tuple i in
                    let get_my_mill =
                        if my_piece == 2 && empty_place == 1
                        then filter_empty_coordonnee_on_three_tuple i can_my_self_mill
                        else can_my_self_mill
                    in
                    let get_block_enemy_mill =
                        if enemy_piece == 2 && empty_place = 1
                        then filter_empty_coordonnee_on_three_tuple i can_block_enemy_mill
                        else can_block_enemy_mill
                    in
                    let get_other_positions =
                        if empty_place > 0
                        then filter_empty_coordonnee_on_three_tuple i other_position
                        else other_position
                    in
                    priority_sort_from_the_board (i + 1) (get_my_mill, get_block_enemy_mill, get_other_positions)
            in
            let ((x, y) : int * int) = priority_sort_from_the_board 0 ([], [], []) in
            Placing (x, y)
        | Moving ->
            (* When the bot is in Moving phase, he chooses a random piece in his bag, and if the piece is not blocked, he moves it to a random direction, else, repeat the operation *)
            let can_my_self_made_mill_without_free_mill_for_my_enemy =
                find_possibilities_of_mill_in_one_move game_update player true
            in
            let opponent = get_opponent game_update player.color in
            let can_enemy_made_mill = find_possibilities_of_mill_in_one_move game_update opponent false in
            let can_block_enemy_mill_and_made_mill_without_free_mill_for_my_enemy =
                intersection_between_two_lists
                  (List.map
                     (fun (coord, move) ->
                       match node_from_direction (get_board game_update) coord move with
                       | Some c -> c
                       | None -> (-1, -1))
                     can_my_self_made_mill_without_free_mill_for_my_enemy)
                  (List.map
                     (fun (coord, move) ->
                       match node_from_direction (get_board game_update) coord move with
                       | Some c -> c
                       | None -> (-1, -1))
                     can_enemy_made_mill)
            in
            if List.length can_block_enemy_mill_and_made_mill_without_free_mill_for_my_enemy <> 0
            then
              let coord, dir =
                  correspond_move_to_go_on
                    (List.nth can_block_enemy_mill_and_made_mill_without_free_mill_for_my_enemy
                       (random (List.length can_block_enemy_mill_and_made_mill_without_free_mill_for_my_enemy)))
                    can_my_self_made_mill_without_free_mill_for_my_enemy game_update
              in
              Moving (coord, dir)
            else if List.length can_my_self_made_mill_without_free_mill_for_my_enemy <> 0
            then
              let coord, dir =
                  List.nth can_my_self_made_mill_without_free_mill_for_my_enemy
                    (random (List.length can_my_self_made_mill_without_free_mill_for_my_enemy))
              in
              Moving (coord, dir)
            else
              let can_my_self_made_mill = find_possibilities_of_mill_in_one_move game_update player false in
              let can_block_enemy_mill_and_made_mill =
                  intersection_between_two_lists
                    (List.map
                       (fun (coord, move) ->
                         match node_from_direction (get_board game_update) coord move with
                         | Some c -> c
                         | None -> (-1, -1))
                       can_my_self_made_mill)
                    (List.map
                       (fun (coord, move) ->
                         match node_from_direction (get_board game_update) coord move with
                         | Some c -> c
                         | None -> (-1, -1))
                       can_enemy_made_mill)
              in
              if List.length can_block_enemy_mill_and_made_mill <> 0
              then
                let coord, dir =
                    correspond_move_to_go_on
                      (List.nth can_block_enemy_mill_and_made_mill
                         (random (List.length can_block_enemy_mill_and_made_mill)))
                      can_my_self_made_mill game_update
                in
                Moving (coord, dir)
              else if List.length can_my_self_made_mill <> 0
              then
                let coord, dir = List.nth can_my_self_made_mill (random (List.length can_my_self_made_mill)) in
                Moving (coord, dir)
              else
                let find_free_my_mill
                    (valid_move : (coordinates * direction_deplacement) list)
                    (current_coordinates : coordinates) =
                    if check_mill_from_position (get_board game_update) current_coordinates player.color
                    then
                      let possible_move = possible_moves_directions game_update current_coordinates player.color in
                      let rec iter_move
                          (valid_move : (coordinates * direction_deplacement) list)
                          (possible_move : direction_deplacement list) =
                          match possible_move with
                          | h :: t -> iter_move ((current_coordinates, h) :: valid_move) t
                          | [] -> valid_move
                      in
                      iter_move valid_move possible_move
                    else valid_move
                in
                let can_free_my_mill = List.fold_left (fun acc coord -> find_free_my_mill acc coord) [] player.bag in
                if List.length can_free_my_mill <> 0
                then
                  let coord, dir = List.nth can_free_my_mill (random (List.length can_free_my_mill)) in
                  Moving (coord, dir)
                else
                  let valid_move = find_all_valid_moves game_update player in
                  let can_block_enemy_mill =
                      intersection_between_two_lists
                        (List.map
                           (fun (coord, move) ->
                             match node_from_direction (get_board game_update) coord move with
                             | Some c -> c
                             | None -> (-1, -1))
                           valid_move)
                        (List.map
                           (fun (coord, move) ->
                             match node_from_direction (get_board game_update) coord move with
                             | Some c -> c
                             | None -> (-1, -1))
                           can_enemy_made_mill)
                  in
                  if List.length can_block_enemy_mill <> 0
                  then
                    let coord, dir =
                        correspond_move_to_go_on
                          (List.nth can_block_enemy_mill (random (List.length can_block_enemy_mill)))
                          valid_move game_update
                    in
                    Moving (coord, dir)
                  else
                    let coord, dir = List.nth valid_move (random (List.length valid_move)) in
                    Moving (coord, dir)
        | Flying ->
            (* When the bot is in Flying phase, he chooses a random square where to place, and repeat that until he finds a correct position, then chooses a random piece in his bag to place it *)
            let opponent = get_opponent game_update player.color in
            let all_free_positions = get_all_free_positions game_update in
            let init_info (can_my_self_made_mill, possible_valid_move, possible_give_mill_to_enemy) current_coordinates
                =
                let rec iter_free_position
                    free_positions
                    (can_my_self_made_mill, possible_valid_move, possible_give_mill_to_enemy) =
                    match free_positions with
                    | h_free_positions :: t_free_positions ->
                        let new_game_update =
                            move_to_coordinates game_update current_coordinates h_free_positions player.color
                        in
                        if can_make_a_mill_in_one_move_at_coord current_coordinates opponent new_game_update
                        then
                          iter_free_position t_free_positions
                            ( can_my_self_made_mill,
                              possible_valid_move,
                              (current_coordinates, h_free_positions) :: possible_give_mill_to_enemy )
                        else if check_mill_from_position (get_board new_game_update) h_free_positions player.color
                        then
                          iter_free_position t_free_positions
                            ( (current_coordinates, h_free_positions) :: can_my_self_made_mill,
                              possible_valid_move,
                              possible_give_mill_to_enemy )
                        else
                          iter_free_position t_free_positions
                            ( can_my_self_made_mill,
                              (current_coordinates, h_free_positions) :: possible_valid_move,
                              possible_give_mill_to_enemy )
                    | [] -> (can_my_self_made_mill, possible_valid_move, possible_give_mill_to_enemy)
                in
                iter_free_position all_free_positions
                  (can_my_self_made_mill, possible_valid_move, possible_give_mill_to_enemy)
            in
            let get_path_to coord flying_list = List.find (fun (_, finish) -> coord = finish) flying_list in

            let can_my_self_made_mill, possible_valid_move, possible_give_mill_to_enemy =
                List.fold_left (fun acc coord -> init_info acc coord) ([], [], []) player.bag
            in
            let can_enemy_made_mill = find_possibilities_of_mill_in_one_move game_update opponent false in
            let can_block_enemy_mill_and_made_mill =
                intersection_between_two_lists
                  (List.map (fun (_, finish) -> finish) can_my_self_made_mill)
                  (List.map
                     (fun (coord, move) ->
                       match node_from_direction (get_board game_update) coord move with
                       | Some c -> c
                       | None -> (-1, -1))
                     can_enemy_made_mill)
            in
            if List.length can_block_enemy_mill_and_made_mill <> 0
            then
              let start, finish =
                  get_path_to
                    (List.nth can_block_enemy_mill_and_made_mill
                       (random (List.length can_block_enemy_mill_and_made_mill)))
                    can_my_self_made_mill
              in
              Flying (start, finish)
            else if List.length can_my_self_made_mill <> 0
            then
              let start, finish = List.nth can_my_self_made_mill (random (List.length can_my_self_made_mill)) in
              Flying (start, finish)
            else if List.length possible_valid_move <> 0
            then
              let start, finish = List.nth possible_valid_move (random (List.length possible_valid_move)) in
              Flying (start, finish)
            else
              let start, finish =
                  List.nth possible_give_mill_to_enemy (random (List.length possible_give_mill_to_enemy))
              in
              Flying (start, finish)
    in
    (* The removing strategy is here *)
    let strategie_remove (game_update : game_update) (player : player) : action =
        let can_my_self_made_mill = find_possibilities_of_mill_in_one_move game_update player false in
        let opponent = get_opponent game_update player.color in
        let can_enemy_made_mill = find_possibilities_of_mill_in_one_move game_update opponent false in
        let free_mill_for_me =
            let get_all_position_of_my_enemy_i_can_free_mill_for_me free_mill_for_me current_coordinates =
                let try_game_update = eliminate_piece game_update current_coordinates opponent.color in
                if can_make_a_mill_in_one_move_at_coord current_coordinates player try_game_update
                then current_coordinates :: free_mill_for_me
                else free_mill_for_me
            in
            List.fold_left
              (fun acc coord -> get_all_position_of_my_enemy_i_can_free_mill_for_me acc coord)
              [] opponent.bag
        in
        let enemy_can_block_my_mill_and_made_mill =
            intersection_between_two_lists
              (List.map
                 (fun (coord, move) ->
                   match node_from_direction (get_board game_update) coord move with
                   | Some c -> c
                   | None -> (-1, -1))
                 can_my_self_made_mill)
              (List.map
                 (fun (coord, move) ->
                   match node_from_direction (get_board game_update) coord move with
                   | Some c -> c
                   | None -> (-1, -1))
                 can_enemy_made_mill)
        in
        if List.length enemy_can_block_my_mill_and_made_mill <> 0
        then
          match
            correspond_move_to_go_on
              (List.nth enemy_can_block_my_mill_and_made_mill
                 (random (List.length enemy_can_block_my_mill_and_made_mill)))
              can_enemy_made_mill game_update
          with
          | c, _ -> Remove c
        else if List.length can_enemy_made_mill <> 0
        then
          Remove
            (List.nth
               (List.map (fun (coord, _) -> coord) can_enemy_made_mill)
               (random (List.length can_enemy_made_mill)))
        else if List.length free_mill_for_me <> 0
        then Remove (List.nth free_mill_for_me (random (List.length free_mill_for_me)))
        else
          let all_valid_enemy_move = find_all_valid_moves game_update opponent in
          let enemy_can_block_my_mill =
              intersection_between_two_lists
                (List.map
                   (fun (coord, move) ->
                     match node_from_direction (get_board game_update) coord move with
                     | Some c -> c
                     | None -> (-1, -1))
                   can_my_self_made_mill)
                (List.map
                   (fun (coord, move) ->
                     match node_from_direction (get_board game_update) coord move with
                     | Some c -> c
                     | None -> (-1, -1))
                   all_valid_enemy_move)
          in
          if List.length enemy_can_block_my_mill <> 0
          then
            match
              correspond_move_to_go_on
                (List.nth enemy_can_block_my_mill (random (List.length enemy_can_block_my_mill)))
                all_valid_enemy_move game_update
            with
            | c, _ -> Remove c
          else Remove (List.nth opponent.bag (random (List.length opponent.bag)))
    in
    { strategie_play; strategie_remove }
