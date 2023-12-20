open Engine

type heurisitc_state = { previous_state: heurisitc_state option; current_state: game_update; ai_to_move: bool }

let get_b board pos =
    match get_square board pos with
    | Some x -> x
    | _ -> failwith "erreur get_b"

let get_num_of_2_peaces state player =
    let board = get_board state.current_state in
    let number = ref 0 in
    List.iter
      (fun (a, b, c) ->
        let a, b, c = List.(assoc a Config.pos, assoc b Config.pos, assoc c Config.pos) in
        match (get_b board a, get_b board b, get_b board c) with
        | Color p1, Color p2, Empty -> if p1 = player.color && p2 = player.color then number := !number + 1
        | Color p1, Empty, Color p2 -> if p1 = player.color && p2 = player.color then number := !number + 1
        | Empty, Color p1, Color p2 -> if p1 = player.color && p2 = player.color then number := !number + 1
        | _ -> ())
      Config.mills;
    !number

let get_num_of_3_peaces state player =
    let board = get_board state.current_state in
    let number = ref 0 in
    List.iter
      (fun (a, b, c, d, e) ->
        let a, b, c, d, e =
            List.(assoc a Config.pos, assoc b Config.pos, assoc c Config.pos, assoc d Config.pos, assoc e Config.pos)
        in
        match (get_b board a, get_b board b, get_b board c, get_b board d, get_b board e) with
        | Color p1, Color p2, Color p3, Empty, Empty when p1 = player.color && p2 = player.color && p3 = player.color ->
            number := !number + 1
        | _ -> ())
      Config.tree;
    !number

let get_num_of_player_double_mill state player =
    let board = get_board state.current_state in
    let number = ref 0 in
    List.iter
      (fun (a, b, c, d, e) ->
        let a, b, c, d, e =
            List.(assoc a Config.pos, assoc b Config.pos, assoc c Config.pos, assoc d Config.pos, assoc e Config.pos)
        in
        match (get_b board a, get_b board b, get_b board c, get_b board d, get_b board e) with
        | Color p1, Color p2, Color p3, Color p4, Color p5
          when [p1; p2; p3; p4; p5] |> List.for_all (fun x -> x = player.color) -> number := !number + 1
        | _ -> ())
      Config.mills2;
    !number

let get_num_of_player_mill state player =
    let board = get_board state.current_state in
    let number = ref 0 in
    List.iter
      (fun (a, b, c) ->
        let a, b, c = List.(assoc a Config.pos, assoc b Config.pos, assoc c Config.pos) in
        match (get_b board a, get_b board b, get_b board c) with
        | Color p1, Color p2, Color p3 ->
            if p1 = player.color && p2 = player.color && p3 = player.color then number := !number + 1
        | _ -> ())
      Config.mills;
    !number

let get_num_of_player_peaces state player =
    let board = get_board state.current_state in
    let number = ref 0 in
    Utils.iter_board
      (fun x ->
        match x with
        | Color c when c = player.color -> number := !number + 1
        | _ -> ())
      board;
    !number

let get_num_of_closed_pieces state player =
    let board = get_board state.current_state in
    let number = ref 0 in
    List.iter
      (fun ((k : int), (v : int list)) ->
        let tmp = ref 0 in
        if get_b board (List.assoc k Config.pos) = Color player.color
        then (
          let break = ref false in
          List.iter
            (fun (adj : int) ->
              if not !break
              then
                if get_b board (List.assoc adj Config.pos) = Empty
                then (
                  tmp := !tmp + 1;
                  break := true)
                else ())
            v;
          if !tmp = 0 then number := !number + 1 else ())
        else ())
      Config.adj;
    !number

let all_player_pieces_closed state player =
    let board = get_board state.current_state in
    let result = ref true in
    List.iter
      (fun ((k : int), (v : int list)) ->
        if get_b board (List.assoc k Config.pos) = player
        then
          List.iter
            (fun adj ->
              if get_b board (List.assoc adj Config.pos) = Empty then result := false else result := !result && true)
            v
        else result := !result && true)
      Config.adj;
    !result

let diff_3_peaces state =
    let black = get_num_of_3_peaces state (get_player_2 state.current_state) in
    let white = get_num_of_3_peaces state (get_player_1 state.current_state) in
    black - white

let diff_double_mills state =
    let black = get_num_of_player_double_mill state (get_player_2 state.current_state) in
    let white = get_num_of_player_double_mill state (get_player_1 state.current_state) in
    black - white

let diff_2_peaces state =
    let black = get_num_of_2_peaces state (get_player_2 state.current_state) in
    let white = get_num_of_2_peaces state (get_player_1 state.current_state) in
    black - white

let diff_closed_peaces state =
    let black = get_num_of_closed_pieces state (get_player_2 state.current_state) in
    let white = get_num_of_closed_pieces state (get_player_1 state.current_state) in
    black - white

let diff_in_mills state =
    let black = get_num_of_player_mill state (get_player_2 state.current_state) in
    let white = get_num_of_player_mill state (get_player_1 state.current_state) in
    black - white

let diff_in_pieces state =
    let black = get_num_of_player_peaces state (get_player_2 state.current_state) in
    let white = get_num_of_player_peaces state (get_player_1 state.current_state) in
    black - white

let winning_config state =
    if all_player_pieces_closed state (Color White)
       || get_num_of_player_peaces state (get_player_1 state.current_state) < 3
    then 1
    else if all_player_pieces_closed state (Color Black)
            || get_num_of_player_peaces state (get_player_2 state.current_state) < 3
    then -1
    else 0

let get_list_of_all_player_mills state player =
    let board = get_board state.current_state in
    let rec aux l acc =
        match l with
        | [] -> acc
        | (x, y, z) :: t -> (
            let a, b, c = List.(assoc x Config.pos, assoc y Config.pos, assoc z Config.pos) in
            match (get_b board a, get_b board b, get_b board c) with
            | Color a, Color b, Color c ->
                if a = player && b == player && c == player then aux t ((x, y, z) :: acc) else aux t acc
            | _ -> aux t acc)
    in
    aux Config.mills []

let mills_has_been_made_in_last_turn state player =
    match state.previous_state with
    | None -> failwith "Erreur"
    | Some prev_st ->
        let prev_mills = get_list_of_all_player_mills prev_st player in
        let curr_mills = get_list_of_all_player_mills state player in
        let rec aux l acc =
            match l with
            | [] -> acc
            | h :: t -> if List.mem h prev_mills then aux t (true || acc) else aux t acc
        in
        aux curr_mills false

let is_white board player = get_player_1 board = player

let closed_morris state ia =
    match state.previous_state with
    | None -> 0
    | Some _ ->
        if state.ai_to_move && ia.color = White
        then
          if mills_has_been_made_in_last_turn state Black
          then -1
          else if mills_has_been_made_in_last_turn state White
          then 1
          else 0
        else 0

let evaluate state (phase : phase) ia =
    match phase with
    | Placing ->
        (18 * closed_morris state ia)
        + (26 * diff_in_mills state)
        + (1 * diff_closed_peaces state)
        + (9 * diff_in_pieces state)
        + (10 * diff_2_peaces state)
        + (7 * diff_3_peaces state)
    | Moving ->
        (14 * closed_morris state ia)
        + (43 * diff_in_mills state)
        + (10 * diff_closed_peaces state)
        + (11 * diff_in_pieces state)
        + (8 * diff_double_mills state)
        + (1086 * winning_config state)
    | Flying ->
        (16 * diff_2_peaces state)
        + (10 * diff_3_peaces state)
        + (1 * closed_morris state ia)
        + (1190 * winning_config state)
