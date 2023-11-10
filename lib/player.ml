open Type
open Board

let init_player (c : Type.color) : player =
    { phase = Placing; color = c; bag = []; piece_placed = 0; nb_pieces_on_board = 0 }

(**This function return a bool if the player can't move *)
let cant_move (player : player) (game : game_update) : bool =
    let rec aux (player : player) (game : game_update) (bag : coordinates list) : bool =
        match bag with
        | [] -> true
        | (x, y) :: xs -> List.length (possible_moves game (x, y) player.color) = 0 && aux player game xs
    in
    aux player game player.bag

(**Player who ramdomly*)
let player_randomly (random : int -> int) : player_strategie =
    let strategie_play (game_update : game_update) (player : player) : move =
        match player.phase with
        | Placing ->
            let rec choise_coord () =
                let i = random (List.length game_update.board) in
                let j = random (List.length game_update.board) in
                match get_square game_update.board (i, j) with
                | Some Empty -> (i,j)
                | _ -> choise_coord ()
            in
            let coord = choise_coord () in
            Placing coord
        | Moving ->
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
            let rec choise_coord () =
                let i = random (List.length game_update.board) in
                let j = random (List.length game_update.board) in
                match get_square game_update.board (i, j) with
                | Some Empty -> (i,j)
                | _ -> choise_coord ()
            in
            let coord_arrive = choise_coord () in
            let i = random (List.length player.bag) in
            let depart = List.nth player.bag i in
            Flying (depart, coord_arrive)
    in
    let strategie_remove (game_update : game_update) (player : player) : coordinates =
        let i = random (List.length (get_opponent game_update player.color).bag) in
        List.nth (get_opponent game_update player.color).bag i
    in
    { strategie_play; strategie_remove }

let lost (game : game_update) (player : player) : bool =
    match player.phase with
    | Moving -> cant_move player game
    | _ -> (player.nb_pieces_on_board <= 2 && player.piece_placed=max_pieces)