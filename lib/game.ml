open Type
open Player
open Board

let quarter =
    [
      [Empty; Path H; Path H; Path H; Path H; Path H; Empty];
      [Path V; Wall; Wall; Wall; Wall; Wall; Path V];
      [Path V; Wall; Empty; Path H; Path H; Path H; Empty];
      [Path V; Wall; Path V; Wall; Wall; Wall; Path V];
      [Path V; Wall; Path V; Wall; Empty; Path H; Empty];
      [Path V; Wall; Path V; Wall; Path V; Wall; Wall];
      [Empty; Path H; Empty; Path H; Empty; Wall; Wall];
    ]

let gameboard = init_board_quarter quarter

let gameRandomly seed =
    let rec play (game : game_update) (player : color) : game_update =
        let () = pretty_print_board game.board in
        if (get_player game player).piece_placed < max_pieces_per_player
        then
          let new_Game_update = play_randomly seed player game Placing in
          play new_Game_update (reverse_color player)
        else if (get_player game player).nb_pieces_on_board <= 3
        then
          if lost game (get_player game player)
          then
            let () = affiche_vainqueur (reverse_color player) in
            game
          else
            let new_Game_update = play_randomly seed player game Flying in
            play new_Game_update (reverse_color player)
        else if lost game (get_player game player)
        then
          let () = affiche_vainqueur (reverse_color player) in
          game
        else
          let new_Game_update = play_randomly seed player game Moving in
          play new_Game_update (reverse_color player)
    in
    let board = gameboard in
    let p1, p2 = (init_player Black, init_player White) in
    let game = { board; mill = false; player1 = p1; player2 = p2; game_is_changed = false } in
    play game Black
