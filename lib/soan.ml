open Engine

let pieces_coord_list (gu : game_update) (p : player) : coordinates list =
    let board = get_board gu in
    List.init 12 (fun i ->
        List.init 12 (fun j ->
            match get_square board (i, j) with
            | Some (Color c) -> if c <> p.color then None else Some (i, j)
            | _ -> None))
    |> List.flatten
    |> List.filter_map (fun coord_opt -> coord_opt)

let actions_affect_mill (gu : game_update) (p : player) (l : action list) : action list =
    List.filter_map
      (fun action ->
        let new_game_update = apply gu p action in
        let new_board = get_board new_game_update in
        let mill_action_opt (color : color) (coord : coordinates) =
            if check_mill_from_position new_board coord color then Some action else None
        in
        match action with
        | Placing c -> c |> mill_action_opt p.color
        | Moving (c, d) -> coordinates_from_directions d c |> mill_action_opt p.color
        | Flying (_, c2) -> c2 |> mill_action_opt p.color
        | Remove c -> c |> mill_action_opt (reverse_color p.color))
      l

let sort_desc_by_function (f : 'a -> 'b) (l : 'a list) = List.fast_sort (fun a b -> compare (f a) (f b)) l |> List.rev

(* Returns the priority of the move, it is the number of adjacent possible moves
   from c on an empty board *)
let nb_adjacent_moves_possible (c : coordinates) =
    let possible_moves = possible_moves_directions (init_game_update Nine_mens_morris) c White in
    possible_moves |> List.length

(* Returns the list of coordinates sorted by priority order in descending order *)
let sort_by_nb_adjacent_moves_possible (l : coordinates list) = sort_desc_by_function nb_adjacent_moves_possible l

let sort_actions_by_priority (l : action list) =
    let priority (action : action) =
        match action with
        | Placing c -> nb_adjacent_moves_possible c
        | Moving (c, d) -> nb_adjacent_moves_possible (coordinates_from_directions d c)
        | Flying (_, c2) -> nb_adjacent_moves_possible c2
        | Remove c -> nb_adjacent_moves_possible c
    in
    sort_desc_by_function priority l

(**
    My player
*)
let bot_SOAN _ (*(random : int -> int)*) : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
           (* Format.printf "ME Placing@."; *)
           (* let () = pretty_print_board (get_board game_update) in *)
            let placing_action =
                let possible_placings = get_all_free_positions game_update in
                let possible_mills =
                    List.map (fun c -> Placing c) possible_placings |> actions_affect_mill game_update player
                in
                if not (List.is_empty possible_mills)
                then possible_mills |> sort_actions_by_priority |> List.hd
                else
                  let opponent = get_opponent game_update player.color in
                  let enemy_pieces_coord_list = pieces_coord_list game_update opponent in
                  let possible_enemy_mills =
                      List.map (fun c -> Placing c) enemy_pieces_coord_list |> actions_affect_mill game_update opponent
                  in
                  if not (List.is_empty possible_enemy_mills)
                  then possible_enemy_mills |> sort_actions_by_priority |> List.hd
                  else List.map (fun c -> Placing c) possible_placings |> sort_actions_by_priority |> List.hd
            in
            (* Format.printf "Placing to ";
               print_cord to_place;
               Format.printf "@."; *)
            placing_action
        | Moving ->
           (* Format.printf "ME Moving@."; *)
           (* let () = pretty_print_board (get_board game_update) in *)

            let moving_action =
                let player_pieces_coord_list = pieces_coord_list game_update player in
                let coord_dirs_list =
                    List.map
                      (fun c -> (c, possible_moves_directions game_update c player.color))
                      player_pieces_coord_list
                in
                let possible_movings =
                    List.map
                      (fun (start_coord, directions) ->
                        let clone_start_coords = List.init (List.length directions) (fun _ -> start_coord) in
                        List.combine clone_start_coords directions)
                      coord_dirs_list
                    |> List.flatten
                in
                let sorted_movings = List.map (fun (c, d) -> Moving (c, d)) possible_movings |> sort_actions_by_priority in
                let possible_mills =
                    sorted_movings |> actions_affect_mill game_update player
                in
                if not (List.is_empty possible_mills)
                then possible_mills |> List.hd
                else
                  let opponent = get_opponent game_update player.color in
                  let enemy_pieces_coord_list = pieces_coord_list game_update opponent in
                  let enemy_coord_dirs_list =
                      List.map
                        (fun c -> (c, possible_moves_directions game_update c opponent.color))
                        enemy_pieces_coord_list
                  in
                  let enemy_possible_movings =
                      List.map
                        (fun (start_coord, directions) ->
                          let clone_start_coords = List.init (List.length directions) (fun _ -> start_coord) in
                          List.combine clone_start_coords directions)
                        enemy_coord_dirs_list
                      |> List.flatten
                  in
                  let enemy_possible_mills =
                    List.map (fun (c, d) -> Moving (c, d)) enemy_possible_movings |> sort_actions_by_priority |> actions_affect_mill game_update opponent
                  in
                  if not (List.is_empty enemy_possible_mills)
                  then enemy_possible_mills |> List.hd
                  else sorted_movings |> List.hd
            in
            (* Format.printf "Moving from ";
            print_cord departure;
            Format.printf " to ";
            print_cord (coordinates_from_directions arrival departure);
            Format.printf "@."; *)
            moving_action
        | Flying ->
           (* Format.printf "ME Flying@."; *)
           (* let () = pretty_print_board (get_board game_update) in *)

            (* Move the piece that has the least adjacent moves possible *)
            let departure_coord =
                pieces_coord_list game_update player |> sort_by_nb_adjacent_moves_possible |> List.rev |> List.hd
            in

            let flying_action =
                let possible_flyings = get_all_free_positions game_update in
                let sorted_flyings = List.map (fun d -> Flying (departure_coord, d)) possible_flyings |> sort_actions_by_priority in
                let possible_mills =
                  sorted_flyings |> actions_affect_mill game_update player
                in
                if not (List.is_empty possible_mills)
                then possible_mills |> List.hd
                else
                  let opponent = get_opponent game_update player.color in
                  let enemy_possible_mills =
                  sorted_flyings |> actions_affect_mill game_update opponent
                  in
                  if not (List.is_empty enemy_possible_mills)
                  then enemy_possible_mills |> List.hd
                  else sorted_flyings |> List.hd
            in 
            (* Format.printf "Flying from ";
            print_cord departure;
            Format.printf " to ";
            print_cord arrival;
            Format.printf "@."; *)
            flying_action
    in

    let strategie_remove (game_update : game_update) (player : player) : action =
      (* Format.printf "ME Removing@."; *)
      (* let () = pretty_print_board (get_board game_update) in *)

        let removing_action =
            let opponent = get_opponent game_update player.color in
            let enemy_pieces_coord_list = pieces_coord_list game_update opponent in
            let possible_enemy_mills =List.map (fun c -> Remove c) enemy_pieces_coord_list |> actions_affect_mill game_update opponent in 
            if List.is_empty possible_enemy_mills
            then List.map (fun r -> Remove r) enemy_pieces_coord_list |> sort_actions_by_priority |> List.hd
            else possible_enemy_mills |> sort_actions_by_priority |> List.hd
        in
        (* Format.printf "Removing ";
        print_cord to_remove;
        Format.printf "@."; *)
        removing_action
    in

    { strategie_play; strategie_remove }
