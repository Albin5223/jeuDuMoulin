open Engine

let check_would_mill_from_position (board : board) ((i, j) : coordinates) (color : color) : bool =
    let rec count_from_dir (x, y) d =
        match node_from_direction board (x, y) d with
        | Some (a, b) -> if get_square board (a, b) = Some (Color color) then 1 + count_from_dir (a, b) d else 0
        | _ -> 0
    in
    let count_row = count_from_dir (i, j) Right + count_from_dir (i, j) Left in
    let count_col = count_from_dir (i, j) Up + count_from_dir (i, j) Down in
    let count_diag1 = count_from_dir (i, j) Up_right + count_from_dir (i, j) Down_left in
    let count_diag2 = count_from_dir (i, j) Up_left + count_from_dir (i, j) Down_right in
    1 + max (max count_row count_col) (max count_diag1 count_diag2) >= (nb_to_get_mill-1)
    

let check_direction_would_mill_from_position (board : board) ((i, j) : coordinates) (color : color) (d:direction_deplacement) : bool =
    check_would_mill_from_position board (coordinates_from_directions d (i,j)) color 

let random = Random.int

let player_ia : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            (* When the bot is in Placing phase, if can mill, will move to do it, else will move randomly *)

            let rec check_mil i j () = 
                if i>(board_length (get_board game_update)) then None 
                else 
                    let a = if j>=(board_length (get_board game_update)) then i+1 else i in
                    let b = if j>=(board_length (get_board game_update)) then 0 else j in
                    match get_square (get_board game_update) (a, b) with
                    | Some Empty ->                    
                        if check_would_mill_from_position (get_board game_update) (a, b) (player.color) then
                        Some (a,b)
                        else 
                            (check_mil (a) (b+1) ())
                    | _ -> check_mil (a) (b+1) ()
            in
             let place_coord ()= 
                match (check_mil 0 0 ()) with
                | Some coord -> coord
                | _ ->
                    let rec choise_coord i j () =
                        let a = if j>=(board_length (get_board game_update)) then i+1 else i in
                        let b = if j>=(board_length (get_board game_update)) then 0 else j in
                        match get_square (get_board game_update) (a, b) with
                        | Some Empty -> (a, b)
                        | _ -> choise_coord (a) (b+1) ()
                    in choise_coord 0 0 ()
                in 
                let coord = place_coord () in
                Placing coord
        | Moving ->
            (* When the bot is in Moving phase, if can mill, will move to do it, else will move randomly *)
            
            let rec check_mill (bag : coordinates list) : action = 
                match bag with 
                | (i,j)::t -> 
                    let possible_move = possible_moves_directions game_update (i,j) player.color in
                    if List.length possible_move = 0
                    then check_mill t
                    else 
                        let rec check_mill_move (possible_moves : direction_deplacement list) : action = 
                        match possible_moves with 
                        | d::t -> 
                            let dirCoords = coordinates_from_directions d (i,j) in
                            if (check_direction_would_mill_from_position (get_board game_update) dirCoords player.color d)  
                            then Moving ((i,j), d)
                            else check_mill_move t
                        | _ -> check_mill t
                        in check_mill_move possible_move
                         
                        | _ -> 
                            let rec choise_mouv () =
                                let i = random (List.length player.bag) in
                                let coord = List.nth player.bag i in
                                let possible_move = possible_moves_directions game_update coord player.color in
                                if List.length possible_move = 0
                                then choise_mouv ()
                                else
                                    let j = random (List.length possible_move) in
                                    let dir = List.nth possible_move j in
                                    Moving (coord, dir)
                            in choise_mouv ()
            in check_mill (player.bag)

            
        | Flying ->
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
            Flying (depart, coord_arrive)
    in
    (* The removing strategy is here *)
    let strategie_remove (game_update : game_update) (player : player) : action =
        let i = random (List.length (get_opponent game_update player.color).bag) in
        Remove (List.nth (get_opponent game_update player.color).bag i)
    in
    { strategie_play; strategie_remove }
