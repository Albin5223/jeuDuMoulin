open Engine

let action_creates_mill game_update color action =
  let newGU = apply game_update (get_player game_update color) action in
  get_is_mill newGU

let rec possible_mill_placing gu c (i, j) =
  if i = (board_length (get_board gu)) then None
  else if get_square (get_board gu) (i, j) = None then possible_mill_placing gu c (i + 1, 0)
  else if get_square (get_board gu) (i, j) = Some Empty && action_creates_mill gu c (Placing (i, j)) then Some (Placing (i, j))
  else possible_mill_placing gu c (i, j + 1)

let rec first_possible_placing board (i, j) =
  if i = (board_length board) then Placing (0, 0)
  else if get_square board (i, j) = None then first_possible_placing board (i + 1, 0)
  else if get_square board (i, j) = Some Empty then Placing (i, j)
  else first_possible_placing board (i, j + 1)

let aux_placing game_update color =
  match possible_mill_placing game_update color (0, 0) with
  | Some (Placing (i, j)) -> Placing (i, j)
  | None -> (match possible_mill_placing game_update (reverse_color color) (0, 0) with
    | Some Placing (i, j) -> Placing (i, j)
    | _ -> first_possible_placing (get_board game_update) (0, 0))
  | _ -> first_possible_placing (get_board game_update) (0, 0)

let rec check_all_moves gu c (i, j) moves =
  match moves with
  |[] -> None
  |x::xs -> let move = Moving ((i, j), x) in
    if action_creates_mill gu c move
    then Some move
    else check_all_moves gu c (i, j) xs

let rec possible_mill_moving game_update color (i, j) =
  if i = (board_length (get_board game_update)) then None
  else if get_square (get_board game_update) (i, j) = None then possible_mill_moving game_update color (i + 1, 0)
  else if get_square (get_board game_update) (i, j) = Some (Color color) then
    let moves = possible_moves_directions game_update (i, j) color in
    let move_option = check_all_moves game_update color (i, j) moves in
    match move_option with
    |Some (Moving _) -> move_option
    |_ -> possible_mill_moving game_update color (i, j + 1)
  else possible_mill_moving game_update color (i, j + 1)

let rec first_possible_move gu c (i, j) =
  if i = (board_length (get_board gu)) then Moving ((0, 0), Up)
  else if get_square (get_board gu) (i, j) = None then first_possible_move gu c (i + 1, 0)
  else if get_square (get_board gu) (i, j) = Some (Color c) then
    let moves = possible_moves_directions gu (i, j) c in
    if moves = [] then first_possible_move gu c (i, j + 1)
    else Moving ((i, j), (List.hd moves))
  else first_possible_move gu c (i, j + 1)

let aux_moving game_update color =
  let move_option = possible_mill_moving game_update color (0, 0) in
  match move_option with
  |Some (Moving (x, y)) -> Moving (x, y)
  |_ -> first_possible_move game_update color (0, 0)

let rec check_all_fly game_update color (i, j) co =
  if i = (board_length (get_board game_update)) then None
  else if get_square (get_board game_update) (i, j) = None then check_all_fly game_update color (i + 1, 0) co
  else if (i, j) = co then check_all_fly game_update color (i, j + 1) co
  else if get_square (get_board game_update) (i, j) = Some Empty && action_creates_mill game_update color (Flying (co, (i, j))) then Some (Flying (co, (i, j)))
  else check_all_fly game_update color (i, j) co

let rec possible_mill_flying game_update color (i, j) =
  if i = (board_length (get_board game_update)) then None
  else if get_square (get_board game_update) (i, j) = None then possible_mill_moving game_update color (i + 1, 0)
  else if get_square (get_board game_update) (i, j) = Some (Color color) then
    let fly_option = check_all_fly game_update color (0, 0) (i, j) in
    match fly_option with
    |Some (Flying _) -> fly_option
    |_ -> possible_mill_flying game_update color (i, j + 1)
  else possible_mill_flying game_update color (i, j + 1)

let rec first_possible_fly gu c (i, j) =
  if i = (board_length (get_board gu)) then Flying ((0, 0), (0, 1))
  else if get_square (get_board gu) (i, j) = None then first_possible_fly gu c (i + 1, 0)
  else if get_square (get_board gu) (i, j) = Some (Color c) then
    let place = first_possible_placing (get_board gu) (0, 0) in
    match place with
    |Placing (x, y) -> Flying ((i, j), (x, y))
    |_ -> Flying ((0, 0), (0, 1))
  else first_possible_fly gu c (i, j + 1)

let aux_flying game_update color =
  let fly_option = possible_mill_flying game_update color (0, 0) in
  match fly_option with
  |Some (Flying (x, y)) -> Flying (x, y)
  |_ -> first_possible_fly game_update color (0, 0)

let rec first_possible_remove game_update color (i, j) =
  if i = (board_length (get_board game_update)) then Remove (0, 0)
  else if get_square (get_board game_update) (i, j) = None then first_possible_remove game_update color (i + 1, 0)
  else if get_square (get_board game_update) (i, j) = Some (Color color) then Remove (i, j)
  else first_possible_remove game_update color (i, j + 1)

let rec aux_remove game_update color (i, j) =
  if i = (board_length (get_board game_update)) then None
  else if get_square (get_board game_update) (i, j) = None then aux_remove game_update color (i + 1, 0)
  else if get_square (get_board game_update) (i, j) = Some (Color color) then
    if check_mill_from_position (get_board game_update) (i, j) color then Some (Remove (i, j))
    else aux_remove game_update color (i, j + 1)
  else aux_remove game_update color (i, j + 1)

let player_georges_lecomte =
  let strategie_play gu p =
    let color = p.color in
    match p.phase with
    |Placing ->
      aux_placing gu color
    |Moving ->
      aux_moving gu color
    |Flying ->
      aux_flying gu color
  in
  let strategie_remove gu p =
    let color = p.color in
    match aux_remove gu (reverse_color color) (0, 0) with
    |Some (Remove x) -> Remove x
    |_ -> first_possible_remove gu (reverse_color color) (0, 0)
  in
  { strategie_play; strategie_remove }
