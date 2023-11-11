open Type

(** Represent the number of pieces that you have to align to get a mill *)
let nb_to_get_mill = 3

(**
  Function that return a none updated game
  This function is used when the move is not legit 
  @param game : the game state  
*)
let not_updated_game game =
    {
      board = game.board;
      mill = false;
      player1 = game.player1;
      player2 = game.player2;
      game_is_changed = false;
      max_pieces = game.max_pieces;
    }

(**
  Returns the coordinates of one step from a coordinate and a direction
  @param d : the direction
  @param ij : the coordinate
*)
let coordinates_from_directions d (i, j) =
    match d with
    | Up -> (i - 1, j)
    | Down -> (i + 1, j)
    | Right -> (i, j + 1)
    | Left -> (i, j - 1)
    | Up_right -> (i - 1, j + 1)
    | Up_left -> (i - 1, j - 1)
    | Down_right -> (i + 1, j + 1)
    | Down_left -> (i + 1, j - 1)

(**
  Function that returns the path that we have to have from a direction : example : if we want to go up, we have to have a path(V) (vertical path)
  @param d : the direction    
*)
let path_to_have_from_direction d =
    match d with
    | Up | Down -> Path V
    | Right | Left -> Path H
    | Up_right | Down_left -> Path DR
    | Up_left | Down_right -> Path DL

(**
  Function to get the square from board at (i,j)
  @param board : the board
  @param ij : the coordinate of the square that we want to get
*)
let get_square (board : board) (i, j) : square option =
    if List.length board = 0
    then None
    else if i >= List.length board || i < 0 || j >= List.length (List.nth board 0) || j < 0
    then None (*of course we know that the board is a square*)
    else Some (List.nth (List.nth board i) j)

let get_square_row (row : square list) (j : int) : square option =
    if List.length row = 0 then None else if j >= List.length row || j < 0 then None else Some (List.nth row j)

(**
  Function that return the row "i" of the board
  @param board : the board
  @param i : the index of the row that we want to get
*)
let get_row (board : board) (i : int) : square list = List.nth board i

(**
  Function that return the column "j" of the board
  @param board : the board
  @param j : the index of the column that we want to get
*)
let get_column (board : board) (j : int) : square list = List.fold_right (fun l acc -> [List.nth l j] @ acc) board []

(**
  Function to get coordinates of a node from another node and a direction. 
  Return None if there is no node in this direction OR if the first step is not a legit Path regarding the direction
  @param board : the board
  @param ij : the coordinate of the departure node
  @param d : the direction of the node that we want to get
*)
let node_from_direction (board : board) ((i, j) : coordinates) (d : direction_deplacement) : coordinates option =
    let rec go_to (board : board) ((x, y) : coordinates) (d : direction_deplacement) : coordinates option =
        let coord_bis = coordinates_from_directions d (x, y) in
        let case = get_square board coord_bis in
        if case = Some (path_to_have_from_direction d)
        then go_to board coord_bis d
        else
          match case with
          | Some Empty | Some (Color _) -> Some coord_bis
          | _ -> None
    in
    if get_square board (coordinates_from_directions d (i, j)) = Some (path_to_have_from_direction d)
    then go_to board (i, j) d
    else None

(**
  Function that check if there is a mill from a certain position (i,j)
  @param board : the board
  @param ij : the coordinate where we want to check if there is a mill
  @param color : the color of the player that wants to check if there is a mill
*)
let check_mill_from_position (board : board) ((i, j) : coordinates) (color : color) : bool =
    match get_square board (i, j) with
    | Some (Color c) when c = color ->
        let rec count_from_dir (x, y) d =
            match node_from_direction board (x, y) d with
            | Some (a, b) -> if get_square board (a, b) = Some (Color color) then 1 + count_from_dir (a, b) d else 0
            | _ -> 0
        in
        let count_row = count_from_dir (i, j) Right + count_from_dir (i, j) Left in
        let count_col = count_from_dir (i, j) Up + count_from_dir (i, j) Down in
        let count_diag1 = count_from_dir (i, j) Up_right + count_from_dir (i, j) Down_left in
        let count_diag2 = count_from_dir (i, j) Up_left + count_from_dir (i, j) Down_right in
        1 + max (max count_row count_col) (max count_diag1 count_diag2) >= nb_to_get_mill
    | _ -> false

(**
  A map that apply the function "f" to the square at the coordinate (i,j) of the board
  @param f : the function to apply to the square
  @param board : the board
  @param (i,j) : the coordinate of the square to change
*)
let board_map (f : square -> square) (board : board) ((i, j) : coordinates) =
    List.mapi (fun x line -> if x = i then List.mapi (fun y el -> if y = j then f el else el) line else line) board

(**
  THIS FUNCTION HAS TO BE PRIVATE :
  Function that put a piece on the board at the coordinate (i,j) and return the new board
  If the position is not legit for a piece, return the old state of the game with the old board
*)
let place_piece_on_board (board : board) ((i, j) : coordinates) color : got_mill =
    let board = board_map (fun x -> if x = Empty then Color color else x) board (i, j) in
    let check = check_mill_from_position board (i, j) color in
    (board, check)

(**
  Function that put a start piece on the board at the coordinate (i,j) and return the new game state
  If the position is not legit for a piece, return the old state of the game
  @param game : the game state
  @param ij : the coordinates where we want to place the piece
  @param color : the color of the player that wants to place the piece
*)
let place_start_piece (game : game_update) ((i, j) : coordinates) (color : color) : game_update =
    let concerned_player = if game.player1.color = color then game.player1 else game.player2 in
    if get_square game.board (i, j) = Some Empty && concerned_player.piece_placed < game.max_pieces
    then
      let board, isMill = place_piece_on_board game.board (i, j) color in
      let updated_player =
          {
            phase = concerned_player.phase;
            color = concerned_player.color;
            piece_placed = concerned_player.piece_placed + 1;
            nb_pieces_on_board = concerned_player.nb_pieces_on_board + 1;
            bag = concerned_player.bag @ [(i, j)];
          }
      in
      if color = game.player1.color
      then
        {
          board;
          mill = isMill;
          player1 = updated_player;
          player2 = game.player2;
          game_is_changed = true;
          max_pieces = game.max_pieces;
        }
      else
        {
          board;
          mill = isMill;
          player1 = game.player1;
          player2 = updated_player;
          game_is_changed = true;
          max_pieces = game.max_pieces;
        }
    else
      {
        board = game.board;
        mill = false;
        player1 = game.player1;
        player2 = game.player2;
        game_is_changed = false;
        max_pieces = game.max_pieces;
      }

(**
  THIS FUNCTION HAS TO BE PRIVATE :
  This function remove a piece from the board and returns it
  Return an unchanged board if there is no piece in (i,j) 
  @param board : the board
  @param (i,j) : the coordinates of the piece to remove
  @param color : the color of the player that gets his piece eliminated (AND NOT THE COLOR OF THE PLAYER THAT ELIMINATES THE PIECE)
*)
let remove_from_board (board : board) ((i, j) : coordinates) color : board =
    board_map (fun x -> if x = Color color then Empty else x) board (i, j)

(**
  This function eliminate a piece from the board and returns the new game state.
  Return an unchanged game if there is no piece in (i,j) or if the piece is not from the player "color"
  By convention, we set the attribute "isMill" to false because we know that there is no mill after an elimination
  @param game : the game state
  @param ij : the coordinates of the piece to eliminate
  @param color : the color of the player that gets his piece eliminated (AND NOT THE COLOR OF THE PLAYER THAT ELIMINATES THE PIECE)
  @return the new game state (with the piece eliminated if the move is legit, else, return the old game state)
*)
let eliminate_piece (game : game_update) ((i, j) : coordinates) (color : color) : game_update =
    match get_square game.board (i, j) with
    | Some (Color c) when c = color ->
        (* We remove the piece and apply the changes for the bag of the concerned player *)
        let concerned_player = get_player game c in
        let new_bag = List.filter (fun (x, y) -> (x, y) <> (i, j)) concerned_player.bag in
        let new_board = remove_from_board game.board (i, j) c in
        let updated_player =
            {
              phase = concerned_player.phase;
              color = concerned_player.color;
              piece_placed = concerned_player.piece_placed;
              nb_pieces_on_board = concerned_player.nb_pieces_on_board - 1;
              bag = new_bag;
            }
        in
        if c = game.player1.color
        then
          {
            board = new_board;
            mill = false;
            player1 = updated_player;
            player2 = game.player2;
            game_is_changed = true;
            max_pieces = game.max_pieces;
          }
        else
          {
            board = new_board;
            mill = false;
            player1 = game.player1;
            player2 = updated_player;
            game_is_changed = true;
            max_pieces = game.max_pieces;
          }
    | _ -> not_updated_game game (* If the piece doesn't exist in (i,j), we do nothing *)

(**
  This function moves a piece from (i1,j1) to (i2,j2)
  Return the changed board if the move is legal, else, return the unchanged board
  @param game : the game state
  @param i1j1 : the coordinate of the piece to move
  @param i2j2 : the coordinate of the destination
  @param color : the color of the player that wants to move the piece
*)
let move_to_coordinates (game : game_update) ((i1, j1) : coordinates) ((i2, j2) : coordinates) (color : color) :
    game_update =
    let arrive = get_square game.board (i2, j2) in
    let depart = get_square game.board (i1, j1) in
    if arrive = Some Empty && depart = Some (Color color)
    then
      let concerned_player = if game.player1.color = color then game.player1 else game.player2 in
      let sub = remove_from_board game.board (i1, j1) concerned_player.color in
      let new_bag = List.map (fun (x, y) -> if (x, y) = (i1, j1) then (i2, j2) else (x, y)) concerned_player.bag in
      let new_board, isMill = place_piece_on_board sub (i2, j2) concerned_player.color in
      let new_player =
          {
            phase = concerned_player.phase;
            color = concerned_player.color;
            piece_placed = concerned_player.piece_placed;
            nb_pieces_on_board = concerned_player.nb_pieces_on_board;
            bag = new_bag;
          }
      in
      if color = game.player1.color
      then
        {
          board = new_board;
          mill = isMill;
          player1 = new_player;
          player2 = game.player2;
          game_is_changed = true;
          max_pieces = game.max_pieces;
        }
      else
        {
          board = new_board;
          mill = isMill;
          player1 = game.player1;
          player2 = new_player;
          game_is_changed = true;
          max_pieces = game.max_pieces;
        }
    else not_updated_game game

(** Init a start board *)
let init_board =
    let rec aux x =
        let rec aux2 i j =
            match (i, j) with
            | 7, _ | _, 7 -> []
            | 3, 3 -> [Wall] @ aux2 i (j + 1)
            | 3, _ | _, 3 -> [Empty] @ aux2 i (j + 1)
            | a, b when a = b || a + b = 6 -> [Empty] @ aux2 i (j + 1)
            | 0, _ | 6, _ | _, 2 | _, 4 -> [Path H] @ aux2 i (j + 1)
            | _ -> [Path V] @ aux2 i (j + 1)
        in

        if x < 7 then aux2 x 0 :: aux (x + 1) else []
    in
    aux 0

(** Function that init a board with only the up-left quarter of it *)
let init_board_quarter (quarter : board) : board =
    let reverse_diagonal el =
        match el with
        | Path DR -> Path DL
        | Path DL -> Path DR
        | _ -> el
    in
    let rec full_board (b : board) (new_board : board) =
        let rec half_row (row : square list) (newRow : square list) =
            match row with
            | [] -> []
            | x :: [] -> newRow @ [x] @ List.rev (List.map reverse_diagonal newRow)
            | x :: xs -> half_row xs (newRow @ [x])
        in
        match b with
        | [] -> []
        | row :: [] ->
            new_board
            @ [half_row row []]
            @ List.rev
                (List.map
                   (fun line ->
                     if List.exists
                          (fun x ->
                            match x with
                            | Path DR | Path DL -> true
                            | _ -> false)
                          line
                     then List.map reverse_diagonal line
                     else line)
                   new_board)
        | row :: rs -> full_board rs (new_board @ [half_row row []])
    in
    full_board quarter []

(** function who tests the maximum of nodes a column can have in order to tell if the point (i,j) can be a path or not *)
let rec test_3_squares_row
    (board : board)
    (width : int)
    (height : int)
    (i : int)
    (j : int)
    (nb_nodes : int)
    (nb_squares : int) : bool =
    if nb_nodes = 3 && j != width / 2
    then
      false
      (*if we already saw 3 nodes and we're not in the middle then we're stopping right here because if we're in the middle we put path(V) everywhere except for the center*)
    else if i < 0
    then
      true
      (*if we reach the end of the board this means we didn't see enough nodes so we can put another path in direction of this last node*)
    else if List.nth (List.nth board i) j = Empty
    then
      test_3_squares_row board width height (i - 1) j (nb_nodes + 1)
        nb_squares (*if we see a node then we increment the nb_nodes for the rest of the board*)
    else test_3_squares_row board width height (i - 1) j nb_nodes nb_squares

let rec aux_init_board (width : int) (height : int) (i : int) (nb_squares : int) (diagonal : bool) (acc : board) : board
    =
    if i > width
    then acc (*base case of the function*)
    else
      (*function that takes width height and a number of squares in argument and returns all the coordinates of the points that board should have, so of course this coordinates form the shape of a square*)
      let rec square_coordinates (w : int) (h : int) (start : int) (nb_squares : int) (acc2 : coordinates list) :
          coordinates list =
          if nb_squares <= 0
          then acc2
          else if width / 2 = start
          then acc2 @ [(start, start)]
          else
            square_coordinates (w - 4) (h - 4) (start + 2) (nb_squares - 1)
              (acc2
              @ [
                  (start, start);
                  (start, start + (w / 2));
                  (start, start + w);
                  (start + (h / 2), start);
                  (start + (h / 2), start + w);
                  (start + h, start);
                  (start + h, start + (w / 2));
                  (start + h, start + w);
                ])
      in
      let square_coord = square_coordinates width height 0 nb_squares [] in
      (*function who creates recursively row in the board with the coordinates of the nodes and connect them with some paths*)
      let rec create_row (j : int) (acc2 : square list) (nb_nodes : int) : square list =
          if j > width
          then acc2
          else if List.mem (i, j) square_coord
          then create_row (j + 1) (acc2 @ [Empty]) (nb_nodes + 1)
          else if ((nb_squares - 1) * 2 < i && i < width - ((nb_squares - 1) * 2))
                  && (nb_squares - 1) * 2 < j
                  && j < width - ((nb_squares - 1) * 2)
          then create_row (j + 1) (acc2 @ [Wall]) nb_nodes
          else if diagonal && i = j
          then create_row (j + 1) (acc2 @ [Path DL]) nb_nodes
          else if diagonal && i = width - j
          then create_row (j + 1) (acc2 @ [Path DR]) nb_nodes
          else if (List.mem (i, j - 1) square_coord || get_square_row acc2 (j - 1) = Some (Path H))
                  && (nb_nodes != 3 || i = width / 2)
          then create_row (j + 1) (acc2 @ [Path H]) nb_nodes
          else if (List.mem (i - 1, j) square_coord || get_square acc (i - 1, j) = Some (Path V))
                  && test_3_squares_row acc width height (i - 1) j 0 nb_squares
          then
            create_row (j + 1) (acc2 @ [Path V]) nb_nodes
            (*if the square above is a node or a path(V) then we create a path(V)
              but only if we didn't have the maximum nodes in the column*)
          else create_row (j + 1) (acc2 @ [Wall]) nb_nodes
      in
      aux_init_board width height (i + 1) nb_squares diagonal (acc @ [create_row 0 [] 0])

(** Function with width height nb_squares and diagonal as arguments and creates a board with *)
let init_board2 (width : int) (height : int) (nb_squares : int) (diagonal : bool) : board =
    if match (width, height, nb_squares) with
       | 4, 4, _ -> true (*three men's morris like a tic-tac-toe; this version is with 2 squares and with diagonals*)
       | 8, 8, _ -> true (*six men's morris; this version is without diagonal and with 2 squares*)
       | 12, 12, _ ->
           true
           (*nine men's morris or twelve men's morris; classic size board but the normal one is without diagonal and with 3 squares*)
       | _, _, _ -> false
    then aux_init_board width height 0 nb_squares diagonal []
    else []

(**
  Function that move a piece from the coordinate (i,j) to a certain direction only if there is a Path in this direction
  Return the changed board if the move is legal, else, return the unchanged board
  @param game : the game state
  @param (i,j) : the coordinate of the piece to move
  @param d : the direction of the move
  @param color : the color of the player that wants to move the piece
*)
let move_to_direction (game : game_update) ((i, j) : coordinates) (d : direction_deplacement) (color : color) :
    game_update =
    match node_from_direction game.board (i, j) d with
    | Some (a, b) -> (
        match get_square game.board (a, b) with
        | Some Empty -> move_to_coordinates game (i, j) (a, b) color
        | _ -> not_updated_game game)
    | _ -> not_updated_game game

(**
  Function to get a list of possible moves from a coordinate (i,j)
  @param game : the game state
  @param (i,j) : the coordinate of the piece
  @param player : the color of the player that wants to move the piece
*)
let possible_moves (game : game_update) ((i, j) : coordinates) (player : color) : direction_deplacement list =
    let rec aux (game : game_update) ((i, j) : coordinates) (player : color) (acc : direction_deplacement list) :
        direction_deplacement list =
        match acc with
        | [] -> []
        | x :: xs -> (
            let destination = node_from_direction game.board (i, j) x in
            match destination with
            | Some (a, b) -> (
                match get_square game.board (a, b) with
                | Some Empty -> x :: aux game (i, j) player xs
                | _ -> aux game (i, j) player xs)
            | _ -> aux game (i, j) player xs)
    in
    aux game (i, j) player [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left]

(**
  Function to apply a move in a game_update
  @param game_update : the game state
  @param player : the player that wants to apply the move
  @param move : the move to apply
*)
let apply (game_update : game_update) (player : player) (move : move) =
    match (move, player.phase) with
    | Placing c, Placing -> place_start_piece game_update c player.color
    | Moving (c, dir), Moving -> move_to_direction game_update c dir player.color
    | Flying (c1, c2), Flying -> move_to_coordinates game_update c1 c2 player.color
    | _ -> not_updated_game game_update

(** Represent the name of defaults board (templates) *)
type template = Three_mens_morris | Six_mens_morris | Nine_mens_morris

(**
  Function that returns the max number of pieces that we can place by each player depending of the template
  @param board : the template of the board
*)
let max_piece_from_template (board : template) : int =
    match board with
    | Three_mens_morris -> 3
    | Six_mens_morris -> 6
    | Nine_mens_morris -> 9

(**
  Function that init a board from a template
  @param template : the template of the board
*)
let init_template (template : template) : board =
    match template with
    | Three_mens_morris -> init_board2 4 4 2 false
    | Six_mens_morris -> init_board2 8 8 2 false
    | Nine_mens_morris -> init_board2 12 12 3 false
