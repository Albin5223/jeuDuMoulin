open Engine
open Heuristic_state

type 'a tree = Node of 'a tree list * 'a

let possible_action state p =
    match p.phase with
    | Placing -> get_all_free_positions state |> List.map (fun x -> Placing x)
    | Moving ->
        p.bag
        |> List.map (fun x -> possible_moves_directions state x p.color |> List.map (fun y -> Moving (x, y)))
        |> List.flatten
    | Flying ->
        p.bag |> List.map (fun x -> get_all_free_positions state |> List.map (fun y -> Flying (x, y))) |> List.flatten

(* J'aurai aimé avoir le temps d'ajouter l'élagage alpha beta *)
let rec minimax depth curr_move curr_state player ai_color : (int * (action * heurisitc_state)) tree =
    if depth <= 1
    then Node ([], (evaluate curr_state player.phase player, (curr_move, curr_state)))
    else
      let list_min_max = if ai_color = player.color then Utils.list_max else Utils.list_min in
      let new_state =
          {
            previous_state = None;
            current_state = apply curr_state.current_state player curr_move;
            ai_to_move = player.color = ai_color;
          }
      in
      let nexts =
          let opponnent = get_opponent new_state.current_state player.color in
          possible_action new_state.current_state opponnent
          |> List.map (fun act -> minimax (depth - 1) act new_state opponnent ai_color)
      in
      let value =
          if List.length nexts <> 0
          then list_min_max (fun (Node (_, (_, (a, _)))) -> a) nexts |> fun (Node (_, (a, (_, _)))) -> a
          else if ai_color = player.color
          then min_int
          else max_int
      in
      Node (nexts, (value, (curr_move, new_state)))

let better_ai move place fly (state : game_update) (player : player) : action =
  (*pretty_print_board (get_board state); *)
    let new_state = { previous_state = None; current_state = state; ai_to_move = true } in
    possible_action state player
    |> List.map (fun action ->
           match player.phase with
           | Placing -> minimax move action new_state player player.color
           | Moving -> minimax place action new_state player player.color
           | Flying -> minimax fly action new_state player player.color)
    |> Utils.list_max (fun (Node (_, (_, (a, _)))) -> a)
    |> fun (Node (_, (_, (action, _)))) -> action

let ai_play (state : game_update) (player : player) : action =
    let prev = { previous_state = None; current_state = state; ai_to_move = player.color <> Black } in
    possible_action state player
    |> List.map (fun x ->
           let new_state =
               { previous_state = Some prev; current_state = apply state player x; ai_to_move = player.color = Black }
           in
           (x, evaluate new_state player.phase player))
    |> Utils.list_max fst
    |> fst

let ai_remove (game_update : game_update) (player : player) : action =
    let prev = { previous_state = None; current_state = game_update; ai_to_move = player.color <> Black } in
    (get_opponent game_update player.color).bag
    |> List.map (fun pos ->
           let new_state =
               {
                 previous_state = Some prev;
                 current_state = apply game_update player (Remove pos);
                 ai_to_move = player.color = Black;
               }
           in
           (Remove pos, evaluate new_state player.phase player))
    |> Utils.list_max (fun (_, x) -> x)
    |> fst

let ai_strategie = { strategie_play = better_ai 2 3 2; strategie_remove = ai_remove }
