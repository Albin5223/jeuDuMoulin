open Engine

let adj_pawn_coord i j (game_update : game_update) plyr =
    let rec adj_pawn_coord_aux i j (game_update : game_update) player dir nb =
        if nb <= 8
        then
          (* check if there are an adjacent pawn with player color in all directions *)
          match dir with
          | Up -> (
              try
                match get_square (get_board game_update) (i - 1, j) with
                (* it's useless to write like that "when player = player" but if I don't do it like that the program doesn't compile and it returns unused value player *)
                | Some player when player = player -> true
                | _ -> adj_pawn_coord_aux i j game_update player Up_right (nb + 1)
              with _ -> adj_pawn_coord_aux i j game_update player Up_right (nb + 1))
          | Up_right -> (
              try
                match get_square (get_board game_update) (i - 1, j + 1) with
                | Some player when player = player -> true
                | _ -> adj_pawn_coord_aux i j game_update player Right (nb + 1)
              with _ -> adj_pawn_coord_aux i j game_update player Right (nb + 1))
          | Right -> (
              try
                match get_square (get_board game_update) (i, j + 1) with
                | Some player when player = player -> true
                | _ -> adj_pawn_coord_aux i j game_update player Down_right (nb + 1)
              with _ -> adj_pawn_coord_aux i j game_update player Down_right (nb + 1))
          | Down_right -> (
              try
                match get_square (get_board game_update) (i + 1, j + 1) with
                | Some player when player = player -> true
                | _ -> adj_pawn_coord_aux i j game_update player Down (nb + 1)
              with _ -> adj_pawn_coord_aux i j game_update player Down (nb + 1))
          | Down -> (
              try
                match get_square (get_board game_update) (i + 1, j) with
                | Some player when player = player -> true
                | _ -> adj_pawn_coord_aux i j game_update player Down_left (nb + 1)
              with _ -> adj_pawn_coord_aux i j game_update player Down_left (nb + 1))
          | Down_left -> (
              try
                match get_square (get_board game_update) (i + 1, j - 1) with
                | Some player when player = player -> true
                | _ -> adj_pawn_coord_aux i j game_update player Left (nb + 1)
              with _ -> adj_pawn_coord_aux i j game_update player Left (nb + 1))
          | Left -> (
              try
                match get_square (get_board game_update) (i, j - 1) with
                | Some player when player = player -> true
                | _ -> adj_pawn_coord_aux i j game_update player Up_left (nb + 1)
              with _ -> adj_pawn_coord_aux i j game_update player Up_left (nb + 1))
          | Up_left -> (
              try
                match get_square (get_board game_update) (i - 1, j - 1) with
                | Some player when player = player -> true
                | _ -> adj_pawn_coord_aux i j game_update player Up (nb + 1)
              with _ -> adj_pawn_coord_aux i j game_update player Up (nb + 1))
        else false
    in
    adj_pawn_coord_aux i j game_update plyr Up 1

let rec dir_align_pawn (game_update : game_update) list_dir coord_dep player =
    match list_dir with
    | Up :: tl ->
        let coord_adj = adj_pawn_coord (fst coord_dep - 1) (snd coord_dep) game_update player in
        if coord_adj then Some Up else dir_align_pawn game_update tl coord_dep player
    | Up_right :: tl ->
        let coord_adj = adj_pawn_coord (fst coord_dep - 1) (snd coord_dep + 1) game_update player in
        if coord_adj then Some Up_right else dir_align_pawn game_update tl coord_dep player
    | Right :: tl ->
        let coord_adj = adj_pawn_coord (fst coord_dep) (snd coord_dep + 1) game_update player in
        if coord_adj then Some Right else dir_align_pawn game_update tl coord_dep player
    | Down_right :: tl ->
        let coord_adj = adj_pawn_coord (fst coord_dep + 1) (snd coord_dep + 1) game_update player in
        if coord_adj then Some Down_right else dir_align_pawn game_update tl coord_dep player
    | Down :: tl ->
        let coord_adj = adj_pawn_coord (fst coord_dep + 1) (snd coord_dep) game_update player in
        if coord_adj then Some Down else dir_align_pawn game_update tl coord_dep player
    | Down_left :: tl ->
        let coord_adj = adj_pawn_coord (fst coord_dep + 1) (snd coord_dep - 1) game_update player in
        if coord_adj then Some Down_left else dir_align_pawn game_update tl coord_dep player
    | Left :: tl ->
        let coord_adj = adj_pawn_coord (fst coord_dep) (snd coord_dep - 1) game_update player in
        if coord_adj then Some Left else dir_align_pawn game_update tl coord_dep player
    | Up_left :: tl ->
        let coord_adj = adj_pawn_coord (fst coord_dep - 1) (snd coord_dep - 1) game_update player in
        if coord_adj then Some Up_left else dir_align_pawn game_update tl coord_dep player
    | _ -> None

let random = Random.int

(* player_amd *)
let player_amd : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            let rec choose () =
                let i = random (board_length (get_board game_update)) in
                let j = random (board_length (get_board game_update)) in
                match get_square (get_board game_update) (i, j) with
                | Some Empty ->
                    let coord_adj = adj_pawn_coord i j game_update player.color in
                    if coord_adj then Placing (i, j) else choose ()
                | _ -> choose ()
            in
            let coordo = choose () in
            coordo
        | Moving ->
            let rec choose () =
                let i = random (List.length player.bag) in
                let coord = List.nth player.bag i in
                let possible_move = possible_moves_directions game_update coord player.color in
                if List.length possible_move = 0
                then choose ()
                else
                  let r = dir_align_pawn game_update possible_move coord player.color in
                  match r with
                  | None ->
                      let j = random (List.length possible_move) in
                      let dir = List.nth possible_move j in
                      Moving (coord, dir)
                  | Some dir -> Moving (coord, dir)
            in
            choose ()
        | Flying ->
            let rec choose () =
                let i = random (board_length (get_board game_update)) in
                let j = random (board_length (get_board game_update)) in
                match get_square (get_board game_update) (i, j) with
                | Some Empty ->
                    let coord_adj = adj_pawn_coord i j game_update player.color in
                    if coord_adj then (i, j) else choose ()
                | _ -> choose ()
            in
            let coord_arrive = choose () in
            let i = random (List.length player.bag) in
            let depart = List.nth player.bag i in
            Flying (depart, coord_arrive)
    in
    let strategie_remove (game_update : game_update) (player : player) : action =
        let i = random (List.length (get_opponent game_update player.color).bag) in
        Remove (List.nth (get_opponent game_update player.color).bag i)
    in
    { strategie_play; strategie_remove }
