open Engine

(* ---------- Possible mill ---------- *)

let action_creates_mill game_update color action =
    let newGU = apply game_update (get_player game_update color) action in
    get_is_mill newGU

let rec possible_mill_placing gu c (i, j) =
    if i = board_length (get_board gu)
    then None
    else if get_square (get_board gu) (i, j) = None
    then possible_mill_placing gu c (i + 1, 0)
    else if get_square (get_board gu) (i, j) = Some Empty && action_creates_mill gu c (Placing (i, j))
    then Some (Placing (i, j))
    else possible_mill_placing gu c (i, j + 1)

let rec check_all_moves gu c (i, j) moves counter =
    match moves with
    | [] -> None
    | x :: xs ->
        let move = Moving ((i, j), x) in
        if counter
        then
          if action_creates_mill gu (reverse_color c) (Placing (coordinates_from_directions x (i, j)))
          then Some move
          else check_all_moves gu c (i, j) xs counter
        else if action_creates_mill gu c move
        then Some move
        else check_all_moves gu c (i, j) xs counter

let rec possible_mill_moving game_update color (i, j) counter =
    if i = board_length (get_board game_update)
    then None
    else if get_square (get_board game_update) (i, j) = None
    then possible_mill_moving game_update color (i + 1, 0) counter
    else if get_square (get_board game_update) (i, j) = Some (Color color)
    then
      let moves = possible_moves_directions game_update (i, j) color in
      let move_option = check_all_moves game_update color (i, j) moves counter in
      match move_option with
      | Some (Moving _) -> move_option
      | _ -> possible_mill_moving game_update color (i, j + 1) counter
    else possible_mill_moving game_update color (i, j + 1) counter

let rec check_all_fly game_update color (i, j) co counter =
    if i = board_length (get_board game_update)
    then None
    else if get_square (get_board game_update) (i, j) = None
    then check_all_fly game_update color (i + 1, 0) co counter
    else if (i, j) = co
    then check_all_fly game_update color (i, j + 1) co counter
    else if counter
    then
      if get_square (get_board game_update) (i, j) = Some Empty
         && action_creates_mill game_update (reverse_color color) (Placing (i, j))
      then Some (Flying (co, (i, j)))
      else check_all_fly game_update color (i, j + 1) co counter
    else if get_square (get_board game_update) (i, j) = Some Empty
            && action_creates_mill game_update color (Flying (co, (i, j)))
    then Some (Flying (co, (i, j)))
    else check_all_fly game_update color (i, j + 1) co counter

let rec possible_mill_flying game_update color (i, j) counter =
    if i = board_length (get_board game_update)
    then None
    else if get_square (get_board game_update) (i, j) = None
    then possible_mill_moving game_update color (i + 1, 0) counter
    else if get_square (get_board game_update) (i, j) = Some (Color color)
    then
      let fly_option = check_all_fly game_update color (0, 0) (i, j) counter in
      match fly_option with
      | Some (Flying _) -> fly_option
      | _ -> possible_mill_flying game_update color (i, j + 1) counter
    else possible_mill_flying game_update color (i, j + 1) counter

let rec set_up_mill game_update color action_list =
    match action_list with
    | [] -> None
    | hd :: tl -> (
        let newGU = apply game_update (get_player game_update color) hd in
        match hd with
        | Placing _ -> (
            match possible_mill_placing newGU color (0, 0) with
            | None -> set_up_mill game_update color tl
            | Some _ -> Some hd)
        | Moving _ -> (
            match possible_mill_moving newGU color (0, 0) false with
            | None -> set_up_mill game_update color tl
            | Some _ -> Some hd)
        | Flying _ -> (
            match possible_mill_flying newGU color (0, 0) false with
            | None -> set_up_mill game_update color tl
            | Some _ -> Some hd)
        | _ -> None)

(* ---------- All possible ---------- *)

let rec all_possible_placing board (i, j) acc =
    if i = board_length board
    then acc
    else if get_square board (i, j) = None
    then all_possible_placing board (i + 1, 0) acc
    else if get_square board (i, j) = Some Empty
    then all_possible_placing board (i, j + 1) (Placing (i, j) :: acc)
    else all_possible_placing board (i, j + 1) acc

let rec all_possible_moves gu c (i, j) acc =
    if i = board_length (get_board gu)
    then acc
    else if get_square (get_board gu) (i, j) = None
    then all_possible_moves gu c (i + 1, 0) acc
    else if get_square (get_board gu) (i, j) = Some (Color c)
    then
      let moves = possible_moves_directions gu (i, j) c in
      all_possible_moves gu c (i, j + 1) (List.map (fun x -> Moving ((i, j), x)) moves @ acc)
    else all_possible_moves gu c (i, j + 1) acc

let rec all_possible_fly gu c (i, j) acc =
    if i = board_length (get_board gu)
    then acc
    else if get_square (get_board gu) (i, j) = None
    then all_possible_fly gu c (i + 1, 0) acc
    else if get_square (get_board gu) (i, j) = Some (Color c)
    then
      let places = all_possible_placing (get_board gu) (0, 0) [] in
      all_possible_fly gu c
        (i, j + 1)
        (List.map
           (fun x ->
             match x with
             | Placing x -> Flying ((i, j), x)
             | _ -> Flying ((i, j), (0, 0)))
           places
        @ acc)
    else all_possible_fly gu c (i, j + 1) acc

let rec all_possible_remove game_update color (i, j) acc =
    if i = board_length (get_board game_update)
    then acc
    else if get_square (get_board game_update) (i, j) = None
    then all_possible_remove game_update color (i + 1, 0) acc
    else if get_square (get_board game_update) (i, j) = Some (Color color)
    then all_possible_remove game_update color (i, j + 1) (Remove (i, j) :: acc)
    else all_possible_remove game_update color (i, j + 1) acc

(* ---------- Aux action ---------- *)

let aux_placing game_update color =
    match possible_mill_placing game_update color (0, 0) with
    | Some (Placing (i, j)) -> Placing (i, j)
    | _ -> (
        match possible_mill_placing game_update (reverse_color color) (0, 0) with
        | Some (Placing (i, j)) -> Placing (i, j)
        | _ -> (
            let all_options = all_possible_placing (get_board game_update) (0, 0) [] in
            match set_up_mill game_update color all_options with
            | Some (Placing (i, j)) -> Placing (i, j)
            | _ ->
                Random.self_init ();
                let i = Random.int (List.length all_options) in
                List.nth all_options i))

let aux_moving game_update color =
    let move_option = possible_mill_moving game_update color (0, 0) false in
    match move_option with
    | Some (Moving (x, y)) -> Moving (x, y)
    | _ -> (
        let counter_mill_option = possible_mill_moving game_update color (0, 0) true in
        match counter_mill_option with
        | Some (Moving (x, y)) -> Moving (x, y)
        | _ -> (
            let all_options = all_possible_moves game_update color (0, 0) [] in
            match set_up_mill game_update color all_options with
            | Some (Moving (x, y)) -> Moving (x, y)
            | _ ->
                Random.self_init ();
                let i = Random.int (List.length all_options) in
                List.nth all_options i))

let aux_flying game_update color =
    let fly_option = possible_mill_flying game_update color (0, 0) false in
    match fly_option with
    | Some (Flying (x, y)) -> Flying (x, y)
    | _ -> (
        let counter_mill_option = possible_mill_flying game_update color (0, 0) true in
        match counter_mill_option with
        | Some (Flying (x, y)) -> Flying (x, y)
        | _ -> (
            let all_options = all_possible_fly game_update color (0, 0) [] in
            match set_up_mill game_update color all_options with
            | Some (Flying (x, y)) -> Flying (x, y)
            | _ ->
                Random.self_init ();
                let i = Random.int (List.length all_options) in
                List.nth all_options i))

let rec aux_remove game_update color (i, j) =
    if i = board_length (get_board game_update)
    then None
    else if get_square (get_board game_update) (i, j) = None
    then aux_remove game_update color (i + 1, 0)
    else if get_square (get_board game_update) (i, j) = Some (Color color)
    then
      if check_mill_from_position (get_board game_update) (i, j) color
      then Some (Remove (i, j))
      else aux_remove game_update color (i, j + 1)
    else aux_remove game_update color (i, j + 1)

(* ---------- Bot ---------- *)

let player_georges_lecomte =
    let strategie_play gu p =
        let color = p.color in
        match p.phase with
        | Placing -> aux_placing gu color
        | Moving -> aux_moving gu color
        | Flying -> aux_flying gu color
    in
    let strategie_remove gu p =
        Random.self_init ();
        let color = p.color in
        match aux_remove gu (reverse_color color) (0, 0) with
        | Some (Remove x) -> Remove x
        | _ ->
            let all_options = all_possible_remove gu (reverse_color color) (0, 0) [] in
            let i = Random.int (List.length all_options) in
            List.nth all_options i
    in
    { strategie_play; strategie_remove }
