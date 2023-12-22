open Engine

let random = Random.int

let smart_player : player_strategie =
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            let rec intersection l1 l2 res =
                match l2 with
                | [] -> res
                | x :: t ->
                    if List.exists (fun i -> i = x) l1 then intersection l1 t res @ [x] else intersection l1 t res
            in
            let block_move =
                intersection (get_all_free_positions game_update)
                  List.[(6, 2); (2, 6); (6, 10); (10, 6); (0, 12); (12, 0); (12, 12); (0, 0)]
                  []
            in
            let possible_opponent_mills =
                let rec aux_possible_opponent_mills l poss_mill =
                    match l with
                    | [] -> poss_mill
                    | h :: t ->
                        if check_mill_from_position
                             (get_board (apply game_update (get_opponent game_update player.color) (Placing h)))
                             h (reverse_color player.color)
                        then aux_possible_opponent_mills t ([h] @ poss_mill)
                        else aux_possible_opponent_mills t poss_mill
                in
                aux_possible_opponent_mills (get_all_free_positions game_update) []
            in
            let possible_mills =
                let rec aux_possible_mills l poss_mill =
                    match l with
                    | [] -> poss_mill
                    | h :: t ->
                        if check_mill_from_position (get_board (apply game_update player (Placing h))) h player.color
                        then aux_possible_mills t ([h] @ poss_mill)
                        else aux_possible_mills t poss_mill
                in
                aux_possible_mills (get_all_free_positions game_update) []
            in
            (* When the bot is in Placing phase, he chooses a random square where to place, and repeat that until he finds a correct position *)
            let rec choise_coord () =
                let i = random (board_length (get_board game_update)) in
                let j = random (board_length (get_board game_update)) in
                match get_square (get_board game_update) (i, j) with
                | Some Empty -> (i, j)
                | _ -> choise_coord ()
            in
            let coord =
                match (possible_opponent_mills, possible_mills, block_move) with
                | [], [], [] -> choise_coord ()
                | x :: _, _, _ -> x
                | _, x :: _, _ -> x
                | _, _, x :: _ -> x
            in
            Placing coord
        | Moving ->
            let coordinates_from_directions_bis d (i, j) =
                match d with
                | Up -> (i - 2, j)
                | Down -> (i + 2, j)
                | Right -> (i, j + 2)
                | Left -> (i, j - 2)
                | Up_right -> (i - 2, j + 2)
                | Up_left -> (i - 2, j - 2)
                | Down_right -> (i + 2, j + 2)
                | Down_left -> (i + 2, j - 2)
            in
            let rec possible_dir coord l poss_dir =
                match l with
                | [] -> poss_dir
                | h :: t ->
                    let app = apply game_update player (Moving (coord, h)) in
                    if check_mill_from_position (get_board app) (coordinates_from_directions_bis h coord) player.color
                    then possible_dir coord t ([(coord, h)] @ poss_dir)
                    else possible_dir coord t poss_dir
            in
            let possible_mills =
                let rec aux_possible_mills l poss_mill =
                    match l with
                    | [] -> poss_mill
                    | h :: t ->
                        aux_possible_mills t (possible_dir h (possible_moves_directions game_update h player.color) [])
                        @ poss_mill
                in
                aux_possible_mills player.bag []
            in
            let rec possible_opps_dir coord l poss_dir =
                match l with
                | [] -> poss_dir
                | h :: t ->
                    let app =
                        apply game_update (get_opponent game_update (reverse_color player.color)) (Moving (coord, h))
                    in
                    if check_mill_from_position (get_board app)
                         (coordinates_from_directions_bis h coord)
                         (reverse_color player.color)
                    then (
                      Format.printf "double teprou";
                      possible_opps_dir coord t ([(coord, h)] @ poss_dir))
                    else possible_opps_dir coord t poss_dir
            in
            let possible_opps_mills =
                let rec aux_possible_opps_mills l poss_mill =
                    match l with
                    | [] -> poss_mill
                    | h :: t ->
                        aux_possible_opps_mills t
                          (possible_opps_dir h
                             (possible_moves_directions game_update h (reverse_color player.color))
                             [])
                        @ poss_mill
                in
                aux_possible_opps_mills (get_opponent game_update player.color).bag []
            in
            let rec get_coord (p : (coordinates * direction_deplacement) list) (l : coordinates list) =
                match p with
                | [] -> l
                | (x, _) :: t -> get_coord t [x] @ l
            in
            let rec auxb coord pdir l =
                match pdir with
                | [] -> l
                | h :: t ->
                    let x = coordinates_from_directions_bis h coord in
                    if List.exists (fun i -> i = x) (get_coord possible_opps_mills [])
                    then auxb coord t [(coord, h)] @ l
                    else auxb coord t l
            in
            let possible_mills_blocking =
                let rec aux_possible_mills_blocking pbag l =
                    match pbag with
                    | [] -> l
                    | h :: t ->
                        aux_possible_mills_blocking t (auxb h (possible_moves_directions game_update h player.color) [])
                        @ l
                in
                aux_possible_mills_blocking player.bag []
            in
            let rec dir_from_mill coord l poss_dir =
                match l with
                | [] -> poss_dir
                | h :: t -> dir_from_mill coord t [(coord, h)] @ poss_dir
            in
            let get_already_mill =
                let rec aux_get_already_mill l poss_mill =
                    match l with
                    | [] -> poss_mill
                    | h :: t ->
                        if check_mill_from_position (get_board game_update) h player.color
                        then
                          aux_get_already_mill t
                            (dir_from_mill h (possible_moves_directions game_update h player.color) [])
                          @ poss_mill
                        else aux_get_already_mill t poss_mill
                in
                aux_get_already_mill player.bag []
            in
            (* When the bot is in Moving phase, he chooses a random piece in his bag, and if the piece is not blocked, he moves it to a random direction, else, repeat the operation *)
            let rec choise_mouv () =
                match (possible_mills, possible_mills_blocking, get_already_mill) with
                | (x, y) :: _, _, _ -> Moving (x, y)
                | _, (x, y) :: _, _ -> Moving (x, y)
                | _, _, (x, y) :: _ -> Moving (x, y)
                | [], [], [] ->
                    let i = random (List.length player.bag) in
                    let coord = List.nth player.bag i in
                    let possible_move = possible_moves_directions game_update coord player.color in
                    if List.length possible_move = 0
                    then choise_mouv ()
                    else
                      let j = random (List.length possible_move) in
                      let dir = List.nth possible_move j in
                      Moving (coord, dir)
            in
            choise_mouv ()
        | Flying -> (
            let possible_opponent_mills =
                let rec aux_possible_opponent_mills l poss_mill =
                    match l with
                    | [] -> poss_mill
                    | h :: t ->
                        if check_mill_from_position
                             (get_board (apply game_update (get_opponent game_update player.color) (Placing h)))
                             h (reverse_color player.color)
                        then aux_possible_opponent_mills t ([h] @ poss_mill)
                        else aux_possible_opponent_mills t poss_mill
                in
                aux_possible_opponent_mills (get_all_free_positions game_update) []
            in
            let possible_mills =
                let rec aux_possible_mills l poss_mill =
                    match l with
                    | [] -> poss_mill
                    | h :: t ->
                        if check_mill_from_position (get_board (apply game_update player (Placing h))) h player.color
                        then aux_possible_mills t ([h] @ poss_mill)
                        else aux_possible_mills t poss_mill
                in
                aux_possible_mills (get_all_free_positions game_update) []
            in
            (* When the bot is in Flying phase, he chooses a random square where to place, and repeat that until he finds a correct position, then chooses a random piece in his bag to place it *)
            let rec choise_coord () =
                let i = random (board_length (get_board game_update)) in
                let j = random (board_length (get_board game_update)) in
                match get_square (get_board game_update) (i, j) with
                | Some Empty -> (i, j)
                | _ -> choise_coord ()
            in
            let coord_arrive = choise_coord () in
            let i = random (List.length player.bag) in
            let depart = List.nth player.bag i in

            match (possible_mills, possible_opponent_mills) with
            | [], [] -> Flying (depart, coord_arrive)
            | _, h :: _ -> Flying (depart, h)
            | h :: _, _ -> Flying (depart, h))
    in
    (* The removing strategy is here *)
    let rec intersection l1 l2 res =
        match l2 with
        | [] -> res
        | x :: t -> if List.exists (fun i -> i = x) l1 then intersection l1 t res @ [x] else intersection l1 t res
    in
    let strategie_remove (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            let possible_opponent_mills =
                let rec aux_possible_opponent_mills l poss_mill =
                    match l with
                    | [] -> poss_mill
                    | (x, y) :: t ->
                        let opps = get_opponent game_update player.color in
                        if check_mill_from_position
                             (get_board (apply game_update opps (Placing (x, y))))
                             (x, y) (reverse_color player.color)
                        then
                          match intersection opps.bag [(x + 2, y); (x, y + 2); (x - 2, y); (x, y - 2)] [] with
                          | [] -> aux_possible_opponent_mills t poss_mill
                          | h :: _ -> aux_possible_opponent_mills t ([h] @ poss_mill)
                        else aux_possible_opponent_mills t poss_mill
                in
                aux_possible_opponent_mills (get_all_free_positions game_update) []
            in
            let i = random (List.length (get_opponent game_update player.color).bag) in
            let rm =
                match possible_opponent_mills with
                | [] -> List.nth (get_opponent game_update player.color).bag i
                | (x, y) :: _ -> (x, y)
            in
            Remove rm
        | Moving ->
            let opp_can_move =
                let rec aux l res =
                    match l with
                    | [] -> res
                    | h :: t ->
                        if List.length (possible_moves_directions game_update h (reverse_color player.color)) = 0
                        then aux t res
                        else aux t [h] @ res
                in
                aux (get_opponent game_update player.color).bag []
            in
            let i = random (List.length (get_opponent game_update player.color).bag) in
            let rm =
                match opp_can_move with
                | [] -> List.nth (get_opponent game_update player.color).bag i
                | (x, y) :: _ -> (x, y)
            in
            Remove rm
        | Flying ->
            let i = random (List.length (get_opponent game_update player.color).bag) in
            Remove (List.nth (get_opponent game_update player.color).bag i)
    in
    { strategie_play; strategie_remove }
