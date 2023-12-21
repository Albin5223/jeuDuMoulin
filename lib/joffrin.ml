open Engine

(* Number of potentially trainable mills at a specified position *)
let get_potentials_mills pos board color =
    let directions = [Up; Down; Right; Left] in

    let evaluate_direction dir =
        match node_from_direction board pos dir with
        | Some new_pos -> (
            match get_square board new_pos with
            | Some (Color c) when c = color ->
                2 (* Higher potential if the adjacent square is a pawn of the same color *)
            | Some Empty -> 1 (* Otherwise if it's an empty box it counts less *)
            | _ -> 0)
        | None -> 0
    in
    List.fold_left (fun acc dir -> acc + evaluate_direction dir) 0 directions

(* List of positions blocking the formation of an opposing mill compared to free positions *)
let get_pos_block_adv board free_positions color =
    let adv_color = reverse_color color in

    let is_blocking_pos (x, y) =
        let check_directions dir1 dir2 =
            (* We are in a central position, we want to look up and down (resp. left and right) *)
            match (node_from_direction board (x, y) dir1, node_from_direction board (x, y) dir2) with
            | Some c1, Some c2 ->
                get_square board c1 = Some (Color adv_color) && get_square board c2 = Some (Color adv_color)
            | _ -> false
        in
        check_directions Up Down
        || check_directions Left Right
        ||
        let check_consecutive dir =
            (* We are on an edge, we want to look at the two consecutive boxes *)
            match node_from_direction board (x, y) dir with
            | Some c1 -> (
                match node_from_direction board c1 dir with
                | Some c2 ->
                    get_square board c1 = Some (Color adv_color) && get_square board c2 = Some (Color adv_color)
                | None -> false)
            | None -> false
        in
        check_consecutive Up || check_consecutive Down || check_consecutive Left || check_consecutive Right
    in
    List.filter is_blocking_pos free_positions

(* Returns a piece (with a direction) to move to reach the specified position *)
let find_piece_to_move game pieces board pos color =
    let rec find_piece = function
        | [] -> None
        | piece :: xs ->
            let possible_directions = possible_moves_directions game piece color in
            if List.exists (fun dir -> node_from_direction board piece dir = Some pos) possible_directions
            then Some (piece, List.find (fun dir -> node_from_direction board piece dir = Some pos) possible_directions)
            else find_piece xs
    in
    find_piece pieces

(* Returns a list of valid moves for a given part *)
let moves_for_bot player game =
    let board = get_board game in
    List.fold_left
      (fun acc piece ->
        let possible_dirs = possible_moves_directions game piece player.color in
        let valid_dirs =
            List.filter
              (fun dir ->
                match node_from_direction board piece dir with
                | Some dest_pos -> (
                    match get_square board dest_pos with
                    | Some Empty -> true (* Destination empty & valid *)
                    | _ -> false (* Invalid destination *))
                | None -> false (* No valid path in direction *))
              possible_dirs
        in
        if valid_dirs <> [] then (piece, valid_dirs) :: acc else acc)
      [] player.bag

(* Returns a valid move at random depending on the phase of the game *)
let random_mouv player game =
    let free_positions = get_all_free_positions game in
    let random_pos = List.nth free_positions (Random.int (List.length free_positions)) in
    match player.phase with
    | Placing -> Placing random_pos
    | Moving ->
        let random_piece, random_dirs =
            List.nth (moves_for_bot player game) (Random.int (List.length (moves_for_bot player game)))
        in
        let rand_dir = List.nth random_dirs (Random.int (List.length random_dirs)) in
        Moving (random_piece, rand_dir)
    | Flying ->
        let random_piece = List.nth player.bag (Random.int (List.length player.bag)) in
        Flying (random_piece, random_pos)

(* We "break" one of our own mills *)
let get_breaking_mill_move game player =
    let board = get_board game in
    let free_positions = get_all_free_positions game in
    let directions = [Up; Down; Right; Left] in

    let is_piece_mill_movable piece =
        check_mill_from_position board piece player.color
        && List.exists
             (fun dir ->
               match node_from_direction board piece dir with
               | Some pos -> List.mem pos free_positions
               | None -> false)
             directions
    in
    let mill_pieces = List.filter is_piece_mill_movable player.bag in

    let get_valid_move_for_piece piece =
        let possible_dirs = possible_moves_directions game piece player.color in
        match possible_dirs with
        | [] -> None
        | dir :: _ -> Some (Moving (piece, dir))
    in

    let rec get_valid_move = function
        | [] -> None
        | piece :: xs -> (
            match get_valid_move_for_piece piece with
            | Some move -> Some move
            | None -> get_valid_move xs)
    in
    get_valid_move mill_pieces

(* We are looking for a possible movement among the list to block an opposing mill *)
let find_valid_blocking_move player game blocking_positions =
    let board = get_board game in
    let pieces = player.bag in

    let rec aux = function
        | [] -> None
        | pos :: rest -> (
            match find_piece_to_move game pieces board pos player.color with
            | Some (piece, dir) -> Some (Moving (piece, dir))
            | None -> aux rest)
    in
    aux blocking_positions

(* We simulate a movement and check if it forms a mill *)
let simulate_and_check_mill_move game player piece dir =
    match node_from_direction (get_board game) piece dir with
    | Some final_position ->
        let simulated_game = move_to_coordinates game piece final_position player.color in
        if get_is_mill simulated_game
        then Some (Moving (piece, dir)) (* Valid movement forming a mill *)
        else None (* Movement not forming a mill *)
    | None -> None (* No valid movement in this direction *)

(* We are trying to form a mill *)
let find_mill_forming_move game player =
    List.fold_left
      (fun acc piece ->
        match acc with
        | Some _ -> acc
        | None ->
            let possible_dirs = possible_moves_directions game piece player.color in
            List.fold_left
              (fun acc dir ->
                match acc with
                | Some _ -> acc
                | None -> simulate_and_check_mill_move game player piece dir)
              None possible_dirs)
      None player.bag

let phase1 game color =
    let board = get_board game in
    let free_positions = get_all_free_positions game in
    let board_size = board_length board in

    (* List of possible positions to create a mill in relation to the pawns already placed *)
    let get_pos_creating_mill board player color immediatly =
        let pieces = player.bag in

        let is_mill_possible_pos (x, y) =
            let check_directions dir1 dir2 =
                (* We are in a central position, we want to look up and down (resp. left and right) *)
                match (node_from_direction board (x, y) dir1, node_from_direction board (x, y) dir2) with
                | Some c1, Some c2 -> (
                    let square1 = get_square board c1 in
                    let square2 = get_square board c2 in
                    match (square1, square2) with
                    | Some (Color c), Some Empty when c = color -> Some c2
                    | Some Empty, Some (Color c) when c = color -> Some c1
                    | Some Empty, Some Empty -> if not immediatly then Some c1 else None
                    | _ -> None)
                | _ -> None
            in
            let check_consecutive dir =
                (* We are on an edge, we want to look at the two consecutive boxes *)
                match node_from_direction board (x, y) dir with
                | Some c1 -> (
                    match node_from_direction board c1 dir with
                    | Some c2 -> (
                        match (get_square board c1, get_square board c2) with
                        | Some (Color c), Some Empty when c = color -> Some c2
                        | Some Empty, Some (Color c) when c = color -> Some c1
                        | Some Empty, Some Empty -> if not immediatly then Some c1 else None
                        | _ -> None)
                    | _ -> None)
                | _ -> None
            in
            match check_directions Up Down with
            | Some pos -> Some pos
            | None -> (
                match check_directions Left Right with
                | Some pos -> Some pos
                | None -> (
                    match check_consecutive Up with
                    | Some pos -> Some pos
                    | None -> (
                        match check_consecutive Down with
                        | Some pos -> Some pos
                        | None -> (
                            match check_consecutive Left with
                            | Some pos -> Some pos
                            | None -> check_consecutive Right))))
        in
        List.find_map is_mill_possible_pos pieces
    in

    (* List of possible positions to attempt the creation of a double mill *)
    let double_mill_positions =
        List.filter
          (fun pos ->
            let potential_after = get_potentials_mills pos board color in
            potential_after > 3)
          free_positions
    in

    (* We are looking to strategically place a pawn to create our mills in the future *)
    let get_opti_placement board free_positions color =
        let rec find_best_position best_pos best_potential = function
            | [] -> best_pos
            | pos :: rest ->
                let potential = get_potentials_mills pos board color in
                if potential > best_potential
                then find_best_position (Some pos) potential rest
                else find_best_position best_pos best_potential rest
        in
        find_best_position None 0 free_positions
    in

    let is_first_move = List.length (get_player game color).bag = 0 in
    if is_first_move (* First movement: we place on the borders *)
    then
      let avantage_positions = [(0, 0); (0, board_size - 1); (board_size - 1, 0); (board_size - 1, board_size - 1)] in
      let valid_positions = List.filter (fun pos -> List.mem pos free_positions) avantage_positions in
      match valid_positions with
      | [] -> Placing (0, 0)
      | pos :: _ -> Placing pos
    else
      match get_pos_block_adv board free_positions color with
      (* We block the opposing mill formation *)
      | pos :: _ -> Placing pos
      | _ -> (
          match get_pos_creating_mill board (get_player game color) color true with
          (* We are looking to form an immediate mill *)
          | Some pos -> Placing pos
          | None -> (
              match double_mill_positions with
              | pos :: _ -> Placing pos (* Place in a double mill position if possible *)
              | [] -> (
                  match get_pos_creating_mill board (get_player game color) color false with
                  (* We are trying to form a mill *)
                  | Some pos -> Placing pos
                  | None -> (
                      match get_opti_placement board free_positions color with
                      (* We seek to place a pawn strategically to prevent opposing mills / create our own in the future *)
                      | Some pos -> Placing pos
                      | None ->
                          let random_pos = List.nth free_positions (Random.int (List.length free_positions)) in
                          (* Otherwise we play randomly *)
                          Placing random_pos))))

let phase2 game player =
    let board = get_board game in
    let free_positions = get_all_free_positions game in

    (* We seek to prevent the formation of an adverse mill in the future *)
    let prevent_opponent_mill game player =
        let board = get_board game in
        let opponent_color = reverse_color player.color in
        let free_positions = get_all_free_positions game in

        let is_blocking_move piece dir =
            match node_from_direction board piece dir with
            | Some dest_pos when List.mem dest_pos free_positions ->
                let potential_before = get_potentials_mills dest_pos board opponent_color in
                potential_before > 1
            | _ -> false
        in

        let find_blocking_move () =
            let rec aux = function
                | [] -> None
                | (piece, dirs) :: rest -> (
                    let valid_dirs = List.filter (fun dir -> is_blocking_move piece dir) dirs in
                    match valid_dirs with
                    | dir :: _ -> Some (Moving (piece, dir))
                    | [] -> aux rest)
            in
            aux (moves_for_bot player game)
        in
        find_blocking_move ()
    in

    (* We are looking to create a mill in the future *)
    let prepare_mill_move game player =
        let board = get_board game in
        let free_positions = get_all_free_positions game in

        let is_mill_forming_move piece dir =
            match node_from_direction board piece dir with
            | Some dest_pos when List.mem dest_pos free_positions ->
                let potential_before = get_potentials_mills piece board player.color in
                let potential_after = get_potentials_mills dest_pos board player.color in
                potential_after > potential_before
            | _ -> false
        in

        let find_best_move () =
            let rec aux = function
                | [] -> None
                | (piece, dirs) :: rest -> (
                    let valid_dirs = List.filter (fun dir -> is_mill_forming_move piece dir) dirs in
                    match valid_dirs with
                    | dir :: _ -> Some (Moving (piece, dir))
                    | [] -> aux rest)
            in
            aux (moves_for_bot player game)
        in
        find_best_move ()
    in

    match find_valid_blocking_move player game (get_pos_block_adv board free_positions player.color) with
    (* We block the opposing mill formation *)
    | Some move -> move
    | None -> (
        (* We are trying to form a mill *)
        match find_mill_forming_move game player with
        | Some move -> move
        | None -> (
            match get_breaking_mill_move game player with
            (* We are trying to break one of our mills (to restore it in the next turn) *)
            | Some move -> move
            | None -> (
                match prepare_mill_move game player with
                (* We are looking to create a mill in the future *)
                | Some move -> move
                | None -> (
                    match prevent_opponent_mill game player with
                    (* We seek to prevent the formation of an adverse mill in the future *)
                    | Some move -> move
                    | None ->
                        (* Otherwise we play randomly *)
                        random_mouv player game))))

let phase3 game player =
    let board = get_board game in
    let free_positions = get_all_free_positions game in

    (* We check if a blocking movement is possible and we carry it out *)
    match find_valid_blocking_move player game (get_pos_block_adv board free_positions player.color) with
    | Some (Moving (piece, _)) ->
        let new_pos = List.find (fun pos -> pos <> piece) free_positions in
        Flying (piece, new_pos)
    | _ -> (
        (* No blocking movement found, we are trying to form a mill *)
        let mill_forming_move = find_mill_forming_move game player in
        match mill_forming_move with
        | Some (Moving (piece, _)) ->
            let new_pos = List.find (fun pos -> pos <> piece) free_positions in
            Flying (piece, new_pos)
        | _ ->
            (* No movement forming a mill, we make a random movement *)
            random_mouv player game)

let remove_strat game player =
    let opponent = get_opponent game player.color in
    let opponent_pieces = opponent.bag in
    let board = get_board game in
    let my_pieces = (get_player game player.color).bag in

    (* We try to remove a piece from an opposing mill *)
    let find_mill_piece () =
        List.find_opt (fun piece -> check_mill_from_position board piece opponent.color) opponent_pieces
    in

    (* We are trying to remove a part that can potentially form a mill *)
    let find_high_potential_piece () =
        List.find_opt
          (fun piece ->
            let potential = get_potentials_mills piece board opponent.color in
            potential >= 2)
          opponent_pieces
    in

    (* We are trying to remove a part that is blocking the formation of our mills *)
    let find_blocking_piece () =
        List.find_opt
          (fun piece ->
            List.exists
              (fun my_piece ->
                let potential = get_potentials_mills my_piece board player.color in
                potential >= 2 && List.mem piece opponent_pieces)
              my_pieces)
          opponent_pieces
    in

    match find_mill_piece () with
    | Some piece -> Remove piece
    | None -> (
        match find_high_potential_piece () with
        | Some piece -> Remove piece
        | None -> (
            match find_blocking_piece () with
            | Some piece -> Remove piece
            | None ->
                let random_index = Random.int (List.length opponent_pieces) in
                Remove (List.nth opponent_pieces random_index)))

let bot_evan =
    let strategie_play game player =
        match player.phase with
        | Placing -> phase1 game player.color
        | Moving -> phase2 game player
        | Flying -> phase3 game player
    in
    let strategie_remove game player = remove_strat game player in
    { strategie_play; strategie_remove }
