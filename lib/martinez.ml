open Engine

let check_mouv_creat_mill game_update coorddep coordarr color =
    let gu = move_to_coordinates game_update coorddep coordarr color in
    check_mill_from_position (get_board gu) coordarr color

let check_place_piece_creat_mill game_update player coord =
    let gu = apply game_update player (Placing coord) in
    check_mill_from_position (get_board gu) coord player.color

let check_fly_creat_mill game_update player coorddep coordarr =
    let gu = apply game_update player (Flying (coorddep, coordarr)) in
    check_mill_from_position (get_board gu) coordarr player.color

let choisir_coord game_update player =
    let listCoord = get_all_free_positions game_update in
    let first = List.hd listCoord in
    let rec choisir_coord_aux listCoord =
        match listCoord with
        | [] -> first
        | x :: xs -> if check_place_piece_creat_mill game_update player x then x else choisir_coord_aux xs
    in
    choisir_coord_aux listCoord

let choisir_coord_arr game_update player coorddep =
    let listCoord = get_all_free_positions game_update in
    let first = List.hd listCoord in
    let rec choisir_coord_aux listCoord =
        match listCoord with
        | [] -> first
        | x :: xs -> if check_fly_creat_mill game_update player coorddep x then x else choisir_coord_aux xs
    in
    choisir_coord_aux listCoord

let without_some some_coord =
    match some_coord with
    | Some (a, b) -> (a, b)
    | None -> (-1, -1)

let choisir_mouv2 game_update player =
    let bag = player.bag in
    let first = List.hd bag in
    let rec choisir_mouv_aux bag tmpcoord tmpdir =
        match bag with
        | [] -> Moving (tmpcoord, tmpdir)
        | coord :: xs ->
            let possible_move = possible_moves_directions game_update coord player.color in
            if List.length possible_move = 0
            then choisir_mouv_aux xs tmpcoord tmpdir
            else
              let rec choisir_mouv_aux2 possible_m retdir =
                  match possible_m with
                  | [] -> choisir_mouv_aux xs coord retdir
                  | dir :: b ->
                      let newcoord = node_from_direction (get_board game_update) coord dir in
                      if newcoord = None
                      then choisir_mouv_aux2 b retdir
                      else if check_mouv_creat_mill game_update coord (without_some newcoord) player.color
                      then Moving (coord, dir)
                      else if check_mill_from_position (get_board game_update) coord player.color
                      then Moving (coord, dir)
                      else choisir_mouv_aux2 b dir
              in
              choisir_mouv_aux2 possible_move (List.hd possible_move)
    in
    choisir_mouv_aux bag first Up

let choisir_depart_arrive game_update player =
    let bag = player.bag in
    let first = List.hd bag in
    let farr = choisir_coord_arr game_update player first in
    let rec choisir_depart_arrive_aux listCoord coorddep coordarr =
        match listCoord with
        | [] -> (coorddep, coordarr)
        | coord :: xs ->
            if not (check_mill_from_position (get_board game_update) coord player.color)
            then
              let arrtmp = choisir_coord_arr game_update player coord in
              if check_fly_creat_mill game_update player coord arrtmp
              then (coord, arrtmp)
              else choisir_depart_arrive_aux xs coorddep coordarr
            else choisir_depart_arrive_aux xs coord (choisir_coord_arr game_update player coord)
    in
    choisir_depart_arrive_aux bag first farr

let strategie_remove (game_update : game_update) (player : player) : action =
    let opp = get_opponent game_update player.color in
    let oppBag = opp.bag in
    let first = List.hd oppBag in
    let rec aux bag supp =
        match bag with
        | [] -> Remove supp
        | coord :: xs ->
            if check_mill_from_position (get_board game_update) coord opp.color
            then aux xs supp
            else
              let possible_move = possible_moves_directions game_update coord opp.color in
              if List.length possible_move = 0
              then aux xs supp
              else
                let rec aux2 possible_m =
                    match possible_m with
                    | [] -> aux xs coord
                    | dir :: b ->
                        let newcoord = node_from_direction (get_board game_update) coord dir in
                        if newcoord = None
                        then aux2 b
                        else if check_mouv_creat_mill game_update coord (without_some newcoord) opp.color
                        then Remove coord
                        else aux2 b
                in
                aux2 possible_move
    in
    aux oppBag first

(**
    Player who plays more smartly
*)
let alec_bot : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            (* When the bot is in Placing phase, he chooses the first square who is empty or a square for create a mill *)
            let coord = choisir_coord game_update player in
            Placing coord
        | Moving ->
            (* When the bot is in Moving phase, he chooses the first piece in his bag who is not blocked, or a piece who can creat a mill *)
            choisir_mouv2 game_update player
        | Flying ->
            (* When the bot is in Flying phase, he first chooses a piece to create a mill, otherwise he moves a piece that is in a mill, or takes the first piece in his bag. *)
            let depart, coord_arrive = choisir_depart_arrive game_update player in
            Flying (depart, coord_arrive)
    in
    { strategie_play; strategie_remove }
