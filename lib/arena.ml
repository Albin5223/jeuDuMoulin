open Type
open Engine

exception Not_Allowed of string

exception Invalid_Strategy of string

(**
  Function that init a strategic player
  @param strategie_play the strategie to play for the player (placing and moving pieces)
  @param strategie_remove the strategie to remove an opponent piece (when a mill is formed)
*)
let init_player_with_strategie
    (strategie_play : game_update -> player -> action)
    (strategie_remove : game_update -> player -> action) =
    { strategie_play; strategie_remove }

(**
  Private function that is useful to play a turn for one player
  @param game_update the game_update
  @param player1 the player that is playing
  @param private_player1 the private player that is playing
  @param private_player2 the private player that is not playing (the opponent)
*)
let private_play game_update (player1 : player_strategie) (private_player1 : player) (private_player2 : player) =
    try
      let move = player1.strategie_play game_update private_player1 in
      let newGU = apply game_update private_player1 move in
      if not newGU.game_is_changed
      then raise (Not_Allowed "Illegal move placed/move")
      else if newGU.mill
      then
        let removed = player1.strategie_remove newGU private_player1 in
        match removed with
        | Remove (x, y) ->
            let newGU = eliminate_piece newGU (x, y) private_player2.color in
            if not newGU.game_is_changed then raise (Not_Allowed "Illegal move remove") else newGU
        | _ -> raise (Invalid_Strategy " Strategy invalid")
      else newGU
    with _ -> raise (Invalid_Strategy "Strategy invalid")

(**
  Function that init the arena between two players, and return the game_update when the game is finished
  @param p1 the first player
  @param p2 the second player
  @param template the template of the board    
*)
let arena (p1 : player_strategie) (p2 : player_strategie) (template : template) : end_game =
    (* we init the private players, p1 is always Black *)
    let private_p1 = init_player White in
    (* we init the private players, p2 is always White *)
    let private_p2 = init_player Black in
    (* recursive function that play a turn for each player *)
    let rec turn (game_update : game_update) =
        (* we update the phase of the players *)
        let game_update = update_phase game_update in
        (* if the player1 has lost, we return the game_update *)
        if lost game_update game_update.player1
        then { winner = private_p2; loser = private_p1; board = game_update.board }
        else
          (* we play the turn for player1 *)
          let newGU = private_play game_update p1 game_update.player1 game_update.player2 in
          (* we update the phase of the players *)
          let newGU = update_phase newGU in
          (* if the player2 has lost, we return the game_update *)
          if lost newGU newGU.player2
          then { winner = private_p1; loser = private_p2; board = game_update.board }
          else
            (* else, we play the turn for player2 *)
            let newGU = private_play newGU p2 newGU.player2 newGU.player1 in
            turn newGU (* we play the next turn *)
    in
    turn
      {
        board = init_board_with_template template;
        mill = false;
        player1 = private_p1;
        player2 = private_p2;
        game_is_changed = false;
        max_pieces = max_piece_from_template template;
      }

(**
    Player who plays ramdomly
    @param random : the seed of the random
*)
let player_random (random : int -> int) : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            (* When the bot is in Placing phase, he chooses a random square where to place, and repeat that until he finds a correct position *)
            let rec choise_coord () =
                let i = random (List.length game_update.board) in
                let j = random (List.length game_update.board) in
                match get_square game_update.board (i, j) with
                | Some Empty -> (i, j)
                | _ -> choise_coord ()
            in
            let coord = choise_coord () in
            Placing coord
        | Moving ->
            (* When the bot is in Moving phase, he chooses a random piece in his bag, and if the piece is not blocked, he moves it to a random direction, else, repeat the operation *)
            let rec choise_mouv () =
                let i = random (List.length player.bag) in
                let coord = List.nth player.bag i in
                let possible_move = possible_moves game_update coord player.color in
                if List.length possible_move = 0
                then choise_mouv ()
                else
                  let j = random (List.length possible_move) in
                  let dir = List.nth possible_move j in
                  Moving (coord, dir)
            in
            choise_mouv ()
        | Flying ->
            (* When the bot is in Flying phase, he chooses a random square where to place, and repeat that until he finds a correct position, then chooses a random piece in his bag to place it *)
            let rec choise_coord () =
                let i = random (List.length game_update.board) in
                let j = random (List.length game_update.board) in
                match get_square game_update.board (i, j) with
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

let rec read (s : string) : int =
    let () = Format.printf "%s@." s in
    try read_int ()
    with Failure _ ->
      let () = print_endline "please enter an int" in
      read s

(**private function*)
let give_direction (dir : direction_deplacement) (l : direction_deplacement list) : string =
    try
      let x = List.find (fun x -> x = dir) l in
      match x with
      | Up_left -> "1  "
      | Up -> " 2 "
      | Up_right -> "  3"
      | Left -> "4  "
      | Right -> "  5"
      | Down_left -> "6  "
      | Down -> " 7 "
      | _ -> "  8"
    with Not_found -> "   "

(**private function*)
let affiche_dir ((i, j) : coordinates) (game : game_update) (player : color) : unit =
    let deplacements = possible_moves game (i, j) player in
    let l1 =
        give_direction Up_left deplacements ^ give_direction Up deplacements ^ give_direction Up_right deplacements
    in
    let l2 = give_direction Left deplacements ^ " x " ^ give_direction Right deplacements in
    let l3 =
        give_direction Down_left deplacements
        ^ give_direction Down deplacements
        ^ give_direction Down_right deplacements
    in
    print_endline l1;
    print_endline l2;
    print_endline l3

let player_human =
    let strategie_play (game_update : game_update) (player : player) : action =
        let () = pretty_print_board game_update.board in
        match player.phase with
        | Placing ->
            let i = read "in which line do you want to place your man ? (i) : " in
            let j = read "in which column do you want to place your man ? : (j) " in
            Placing (traductor (i, j) (List.length (List.nth game_update.board 0) - 1))
        | Moving ->
            let get_cord =
                let i = read "in which line is the man you want to move ? (i) : " in
                let j = read "in which column is the man you want to move ? : (j) " in
                traductor (i, j) (List.length (List.nth game_update.board 0) - 1)
            in

            let rec deplace ((i, j) : coordinates) (col : color) =
                let () = affiche_dir (i, j) game_update col in
                let intDir = read "what direction do you choose ? : " in
                try
                  match intDir with
                  | 1 -> Up_left
                  | 2 -> Up
                  | 3 -> Up_right
                  | 4 -> Left
                  | 5 -> Right
                  | 6 -> Down_left
                  | 7 -> Down
                  | 8 -> Down_right
                  | _ -> raise Not_found
                with Not_found ->
                  let () = raise (Not_Allowed "movement impossible ") in
                  deplace (i, j) col
            in
            let coord = get_cord in
            let dir = deplace coord player.color in
            Moving (coord, dir)
        | Flying ->
            let rec choice_start board =
                let i = read "in which line you want to move it ? (i) : " in
                let j = read "in which column you want to move it ? : (j) " in
                try
                  match get_square board (traductor (i, j) (List.length (List.nth game_update.board 0))) with
                  | Some (Color x) ->
                      if x = player.color
                      then traductor (i, j) (List.length (List.nth game_update.board 0))
                      else raise (Not_Allowed " You can't move an enemy man")
                  | _ -> raise (Not_Allowed " Case is not valid")
                with Not_Allowed x ->
                  let () = Format.printf "%s@." x in
                  choice_start board
            in

            let rec choice_end board =
                let i = read "in which line is the man you want to move ? (i) : " in
                let j = read "in which column is the man you want to move ? : (j) " in
                try
                  match get_square board (traductor (i, j) (List.length (List.nth game_update.board 0))) with
                  | Some Empty -> traductor (i, j) (List.length (List.nth game_update.board 0))
                  | _ -> raise (Not_Allowed " Case is not Empty")
                with Not_Allowed x ->
                  let () = Format.printf "%s@." x in
                  choice_end board
            in

            Flying (choice_start game_update.board, choice_end game_update.board)
    in
    let rec strategie_remove (game_update : game_update) (player : player) : action =
        let i = read "in which line is the man you want to remove ? (i) : " in
        let j = read "in which column is the man you want to remove ? : (j) " in
        let coord = traductor (i, j) (List.length (List.nth game_update.board 0)) in
        try
          match get_square game_update.board coord with
          | Some (Color x) ->
              if x = reverse_color player.color
              then Remove (traductor (i, j) (List.length (List.nth game_update.board 0) - 1))
              else raise (Not_Allowed " You can't destruct your own man")
          | _ -> raise (Not_Allowed "")
        with Not_Allowed x ->
          let () = Format.printf "%s@." x in
          strategie_remove game_update player
    in

    { strategie_play; strategie_remove }
