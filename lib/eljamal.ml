open Engine

let (last_act : action list ref) = ref []

let record_move act =
    match !last_act with
    | [_; a2] -> last_act := [a2; act] (* garde la dernière action effectuée et la nouvelle *)
    | _ -> last_act := !last_act @ [act]
(* ajoute la nouvelle act *)

let is_repeating_moves act =
    match !last_act with
    | [a1; a2] -> if a1 = act || a2 = act then true else false
    | _ -> false

(**trie toute les positions par ordre de mouvement possibles a partir de celle ci*)
let sort_pos game_update lst =
    let is_valid_pos pos_opt =
        match pos_opt with
        | Some _ -> true
        | None -> false
    in
    let all_dep = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
    let board = get_board game_update in
    let num_possible_moves pos =
        List.length (List.filter (fun dir -> is_valid_pos (node_from_direction board pos dir)) all_dep)
    in
    List.sort (fun pos1 pos2 -> compare (num_possible_moves pos2) (num_possible_moves pos1)) lst

(**fonction pour trouver toutes les positions où le joueur peut placer une pièce pour former un mill*)
let pos_for_mill_placing game_update player =
    let possible_pos = get_all_free_positions game_update in
    List.filter
      (fun p -> check_mill_from_position (get_board (apply game_update player (Placing p))) p player.color)
      possible_pos

(**renvoie toute les positions ou il y a déja un mill*)
let all_mill_positions game_update player =
    let board = get_board game_update in
    let length = board_length board in
    let all_positions = List.init length (fun i -> List.init length (fun j -> (i, j))) |> List.flatten in
    List.filter (fun pos -> check_mill_from_position board pos player.color) all_positions

(**calcule les nouvelles positions après avoir effectué des déplacements à partir d'une position donnée*)
let pos_of_move pos direction_deplacements =
    let aux pos deplacement =
        match deplacement with
        | Up -> (fst pos, snd pos - 1)
        | Down -> (fst pos, snd pos + 1)
        | Right -> (fst pos + 1, snd pos)
        | Left -> (fst pos - 1, snd pos)
        | Up_right -> (fst pos + 1, snd pos - 1)
        | Up_left -> (fst pos - 1, snd pos - 1)
        | Down_right -> (fst pos + 1, snd pos + 1)
        | Down_left -> (fst pos - 1, snd pos + 1)
    in
    List.map (aux pos) direction_deplacements

(**renvoi le mouvment nécessaire pour aller d'une position à une autre adjacente*)
let move_to_adjacent_pos pos1 target =
    let dx = fst target - fst pos1 in
    let dy = snd target - snd pos1 in
    match (dx, dy) with
    | 1, 0 -> Some Right
    | -1, 0 -> Some Left
    | 0, 1 -> Some Down
    | 0, -1 -> Some Up
    | 1, -1 -> Some Up_right
    | -1, -1 -> Some Up_left
    | 1, 1 -> Some Down_right
    | -1, 1 -> Some Down_left
    | _ -> None

(**renvoi toutes les positions ou le joueur peut déplacer un pion *)
let get_all_possible_moves game_update player =
    let rec aux pos_list acc =
        match pos_list with
        | [] -> acc
        | pos :: rest ->
            let possible_moves = pos_of_move pos (possible_moves_directions game_update pos player.color) in
            aux rest (List.append acc possible_moves)
    in
    aux player.bag []

(**renvoie la première position d'une liste donnée qui est incluse dans les mouvements possibles d'un joueur*)
let first_pos_in_possible_moves game_update player pos_list =
    let possible_moves = get_all_possible_moves game_update player in
    let rec aux pos_list =
        match pos_list with
        | [] -> None
        | pos :: rest -> if List.mem pos possible_moves then Some pos else aux rest
    in
    aux pos_list

(**renvoie la position de départ et le déplacement nécessaire pour atteindre une position d'arrivée spécifique*)
let get_move_to_pos game_update player target_pos =
    let rec aux pos_list =
        match pos_list with
        | [] -> None
        | pos :: rest ->
            let possible_moves = pos_of_move pos (possible_moves_directions game_update pos player.color) in
            if List.mem target_pos possible_moves
            then
              let move = move_to_adjacent_pos pos target_pos in
              match move with
              | None -> None
              | Some move -> Some (pos, move)
            else aux rest
    in
    aux player.bag

(**teste si le deplacement a une position cree un mill*)
let is_moving_for_mill game_update player act =
    match act with
    | Moving (pos, dir) -> (
        let new_gu = apply game_update player (Moving (pos, dir)) in
        let p_opt = node_from_direction (get_board new_gu) pos dir in
        match p_opt with
        | None -> false
        | Some p -> check_mill_from_position (get_board new_gu) p player.color)
    | _ -> false

(**renvoi true si il n'y a pas d'adversaire à une case adjacente d'une pos, false sinon*)
let is_not_opponent_in_adj game_update player pos =
    let board = get_board game_update in
    let all_dep = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
    let rec aux pos_list =
        match pos_list with
        | [] -> true
        | pos :: rest -> (
            let node = node_from_direction board pos Up in
            match node with
            | None -> aux rest
            | Some n -> (
                let square = get_square board n in
                match square with
                | None -> aux rest
                | Some s -> (
                    match s with
                    | Color c -> if c = (get_opponent game_update player.color).color then false else aux rest
                    | _ -> aux rest)))
    in

    aux (pos_of_move pos all_dep)

(**renvoi la liste des positions ou le joueur peut déplacer un pion pour former un mill*)
let pos_for_mill_moving game_update player =
    let all_move_pos = get_all_possible_moves game_update player in
    let check_mill_from_move gu player (pos, dir) =
        let new_gu = apply gu player (Moving (pos, dir)) in
        let p_opt = node_from_direction (get_board new_gu) pos dir in
        match p_opt with
        | None -> false
        | Some p -> check_mill_from_position (get_board new_gu) p player.color
    in
    let move_lst = List.map (fun pos -> get_move_to_pos game_update player pos) all_move_pos in
    let move_lst = List.filter (fun m -> m <> None) move_lst in
    let filt_lst = List.filter (fun m -> check_mill_from_move game_update player (Option.get m)) move_lst in
    List.map (fun m -> Option.get m) filt_lst

(**retourne la liste des positions vides adjacentes à une pos *)
let all_empty_pos_adj game_update pos =
    let board = get_board game_update in
    let all_dep = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
    let rec aux pos_list acc =
        match pos_list with
        | [] -> acc
        | pos :: rest -> (
            let node = node_from_direction board pos Up in
            match node with
            | None -> aux rest acc
            | Some n -> (
                let square = get_square board n in
                match square with
                | None -> aux rest acc
                | Some s -> (
                    match s with
                    | Empty -> aux rest (acc @ [pos])
                    | _ -> aux rest acc)))
    in

    aux (pos_of_move pos all_dep) []

(**renvoi un déplacement au hasard parmi ceux possibles*)
let rec random_move game_update player =
    let all_possible_mv = get_all_possible_moves game_update player in
    Random.self_init ();
    let random_pos = List.nth all_possible_mv (Random.int (List.length all_possible_mv)) in
    match get_move_to_pos game_update player random_pos with
    | None -> random_move game_update player
    | Some (pos, dir) -> Moving (pos, dir)

(**renvoi la liste de toutes les positions vides adjacentes à un mill et qui ne sont pas adjacentes à un adversaire*)
let empty_not_opp_adj_with_mill game_update player =
    let all_mill_pos = all_mill_positions game_update player in
    let mill_without_opp_in_adj = List.filter (fun p -> is_not_opponent_in_adj game_update player p) all_mill_pos in
    List.flatten (List.map (fun p -> all_empty_pos_adj game_update p) mill_without_opp_in_adj)

(**trouver toutes les positions vides sur le plateau où le joueur peut alterner entre deux moulins a chaque coup*)
let pos_for_2_mill game_update player =
    let all_mill_pos = all_mill_positions game_update player in
    let all_empty_adj_mill = List.flatten (List.map (fun p -> all_empty_pos_adj game_update p) all_mill_pos) in
    let all_dep = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
    let move_cause_mill pos dep =
        let new_gu = apply game_update player (Moving (pos, dep)) in
        let p_opt = node_from_direction (get_board new_gu) pos dep in
        match p_opt with
        | None -> false
        | Some p -> check_mill_from_position (get_board new_gu) p player.color
    in
    let l =
        List.filter
          (fun (pos, dep) -> move_cause_mill pos dep)
          (List.flatten (List.map (fun pos -> List.map (fun dep -> (pos, dep)) all_dep) all_empty_adj_mill))
    in
    List.map (fun (pos, _) -> pos) l

let placing_strat game_update player =
    (*regarde si il y a une position ou l'adversaire peut faire un mill*)
    let defense_pos = pos_for_mill_placing game_update (get_opponent game_update player.color) in
    if List.length defense_pos > 0
    then
      let sort_defense_pos = sort_pos game_update defense_pos in
      Placing (List.hd sort_defense_pos)
    else
      (*regarde si il y a une position ou on peut faire un mill*)
      let attack_pos = pos_for_mill_placing game_update player in
      if List.length attack_pos > 0
      then
        let sort_attack_pos = sort_pos game_update attack_pos in
        Placing (List.hd sort_attack_pos)
      else
        (*prend la position avec le plus de cases adjacentes*)
        let possible_pos = get_all_free_positions game_update in
        let sort_possible_pos = sort_pos game_update possible_pos in
        Placing (List.hd sort_possible_pos)

let moving_strat game_update player =
    let rec real_moving_strat game_update player i =
        (*regarde si il y a une position ou on peut alterner entre 2 mill*)
        let attack_pos1 = pos_for_2_mill game_update player in
        if List.length attack_pos1 > 0 && i < 1
        then
          let sort_attack_pos1 = sort_pos game_update attack_pos1 in
          let pos = first_pos_in_possible_moves game_update player sort_attack_pos1 in
          match pos with
          | None -> real_moving_strat game_update player (i + 1)
          | Some p -> (
              match get_move_to_pos game_update player p with
              | None -> real_moving_strat game_update player (i + 1)
              | Some (pos, dir) ->
                  if is_moving_for_mill game_update player (Moving (pos, dir))
                  then Moving (pos, dir)
                  else real_moving_strat game_update player (i + 1))
        else
          (*regarde si il y a une position ou on peut faire un mill*)
          let attack_pos2 = pos_for_mill_moving game_update player in
          if List.length attack_pos2 > 0
          then Moving (fst (List.hd attack_pos2), snd (List.hd attack_pos2))
          else
            (*regarde si l'adversaire a un pion d'un mill qu'il peut déplacer de sorte à le faire revenir dans le mill au prochain tour*)
            let move_and_return_opp = empty_not_opp_adj_with_mill game_update (get_opponent game_update player.color) in
            if List.length move_and_return_opp > 0 && i < 2
            then
              let sort_move_and_return_opp = sort_pos game_update move_and_return_opp in
              let pos = first_pos_in_possible_moves game_update player sort_move_and_return_opp in
              match pos with
              | None -> real_moving_strat game_update player (i + 1)
              | Some p -> (
                  match get_move_to_pos game_update player p with
                  | None -> real_moving_strat game_update player (i + 1)
                  | Some (pos, dir) -> Moving (pos, dir))
            else
              (*regarde si il y a une position ou l'adversaire peut alterner entre 2 mill*)
              let defense_pos1 = pos_for_2_mill game_update (get_opponent game_update player.color) in
              if List.length defense_pos1 > 0 && i < 3
              then
                let sort_defense_pos1 = sort_pos game_update defense_pos1 in
                let pos = first_pos_in_possible_moves game_update player sort_defense_pos1 in
                match pos with
                | None -> real_moving_strat game_update player (i + 1)
                | Some p -> (
                    match get_move_to_pos game_update player p with
                    | None -> real_moving_strat game_update player (i + 1)
                    | Some (pos, dir) -> Moving (pos, dir))
              else
                (*regarde si il y a une position ou l'adversaire peut faire un mill*)
                let defense_pos2 = pos_for_mill_moving game_update (get_opponent game_update player.color) in
                if List.length defense_pos2 > 0 && i < 4
                then
                  let pos =
                      node_from_direction (get_board game_update)
                        (fst (List.hd defense_pos2))
                        (snd (List.hd defense_pos2))
                  in
                  match pos with
                  | None -> real_moving_strat game_update player (i + 1)
                  | Some p -> (
                      match get_move_to_pos game_update player p with
                      | None -> real_moving_strat game_update player (i + 1)
                      | Some (pos, dir) -> Moving (pos, dir))
                else
                  (*regarde si il y a un pion d'un mill que l'on peut déplacer de sorte à le faire revenir dans le mill au prochain tour*)
                  let move_and_return = empty_not_opp_adj_with_mill game_update player in
                  if List.length move_and_return > 0 && i < 5
                  then
                    let sort_move_and_return = sort_pos game_update move_and_return in
                    let pos = first_pos_in_possible_moves game_update player sort_move_and_return in
                    match pos with
                    | None -> real_moving_strat game_update player (i + 1)
                    | Some p -> (
                        match get_move_to_pos game_update player p with
                        | None -> real_moving_strat game_update player (i + 1)
                        | Some (pos, dir) -> Moving (pos, dir))
                  else
                    (*se deplace à la position avec le plus de mouvements possibles dans la liste des deplacements possibles*)
                    let all_possible_mv = get_all_possible_moves game_update player in
                    let sort_all_possible_mv = sort_pos game_update all_possible_mv in
                    let pos = List.hd sort_all_possible_mv in
                    match get_move_to_pos game_update player pos with
                    | None ->
                        Moving
                          ( List.hd player.bag,
                            List.hd (possible_moves_directions game_update (List.hd player.bag) player.color) )
                    | Some (pos, dir) ->
                        if is_repeating_moves (Moving (pos, dir))
                        then random_move game_update player
                        else Moving (pos, dir)
    in

    real_moving_strat game_update player 0

(*
(**pretty printer d'action moving et flying*)
let pp_act action =
    let string_of_direction dir =
        match dir with
        | Up -> "Up"
        | Down -> "Down"
        | Right -> "Right"
        | Left -> "Left"
        | Up_right -> "Up_right"
        | Up_left -> "Up_left"
        | Down_right -> "Down_right"
        | Down_left -> "Down_left"
    in
    match action with
    | Moving (pos, dir) -> Printf.printf "Moving (%d, %d) %s\n" (fst pos) (snd pos) (string_of_direction dir)
    | Flying (pos1, pos2) -> Printf.printf "Flying (%d, %d) (%d, %d)\n" (fst pos1) (snd pos1) (fst pos2) (snd pos2)
    | _ -> ()
*)

(**renvoie la liste des positions ou le joueur peut faire un mill en flying*)
let flying_for_mill game_update player =
    let possible_moves = get_all_free_positions game_update in
    let all_current_pos = player.bag in
    let fly_cause_mill pos1 pos2 =
        let new_gu = apply game_update player (Flying (pos1, pos2)) in
        match get_square (get_board new_gu) pos2 with
        | None -> false
        | Some s -> (
            match s with
            | Color _ -> check_mill_from_position (get_board new_gu) pos2 player.color
            | _ -> false)
    in
    List.filter
      (fun (pos1, pos2) -> fly_cause_mill pos1 pos2)
      (List.flatten (List.map (fun pos1 -> List.map (fun pos2 -> (pos1, pos2)) possible_moves) all_current_pos))

let flying_strat game_update player =
    (*regarde si il y a une position ou on peut faire 2 mill*)
    let attack_pos1 = pos_for_2_mill game_update player in
    if List.length attack_pos1 > 0
    then
      let sort_attack_pos1 = sort_pos game_update attack_pos1 in
      Flying (List.hd player.bag, List.hd sort_attack_pos1)
    else
      (*regarde si il y a une position ou on peut faire un mill*)
      let attack_pos2 = flying_for_mill game_update player in
      if List.length attack_pos2 > 0
      then
        let p1, p2 = List.hd attack_pos2 in
        Flying (p1, p2)
      else
        (*regarde si il y a une position ou l'adversaire peut faire 2 mill*)
        let defense_pos1 = pos_for_2_mill game_update (get_opponent game_update player.color) in
        if List.length defense_pos1 > 0
        then
          let sort_defense_pos1 = sort_pos game_update defense_pos1 in
          Flying (List.hd player.bag, List.hd sort_defense_pos1)
        else
          (*regarde si il y a une position ou l'adversaire peut faire un mill*)
          let defense_pos2 = pos_for_mill_moving game_update (get_opponent game_update player.color) in
          if List.length defense_pos2 > 0
          then
            let pos =
                node_from_direction (get_board game_update) (fst (List.hd defense_pos2)) (snd (List.hd defense_pos2))
            in
            match pos with
            | None -> Flying (List.hd player.bag, List.hd (get_all_free_positions game_update))
            | Some p -> Flying (List.hd player.bag, p)
          else
            (*se deplace à la position avec le plus de mouvements possibles dans la liste des deplacements possibles*)
            let sort_all_possible_mv = sort_pos game_update (get_all_free_positions game_update) in
            Flying (List.hd player.bag, List.hd sort_all_possible_mv)

let remove_strat game_update player =
    Random.self_init ();
    let opp = get_opponent game_update player.color in
    (*cherche les positions ou l'adversaire peut faire un mill au prochain tour*)
    let all_move_do_mill_opp = pos_for_mill_moving game_update opp in
    if List.length all_move_do_mill_opp > 0
    then Remove (fst (List.hd all_move_do_mill_opp)) (*remove au hasard sinon*)
    else Remove (List.nth (sort_pos game_update opp.bag) (Random.int (List.length opp.bag)))

let player_ali =
    let strategie_play (game_update : game_update) (player : player) : action =
        (*pretty_print_board (get_board game_update);
          pretty_print_phase player.phase;*)
        match player.phase with
        | Placing -> placing_strat game_update player
        | Moving ->
            let m = moving_strat game_update player in
            (*pp_act m;*)
            record_move m;
            m
        | Flying -> flying_strat game_update player
    in
    let strategie_remove (game_update : game_update) (player : player) : action = remove_strat game_update player in
    { strategie_play; strategie_remove }
