open Type
open Board
open Player

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

let gameboard = initBoardQuarter quarter

let gameRandomly seed =
    let rec play (game : gameUpdate) (player : color) : gameUpdate =
        let () = prettyPrintBoard game.board in
        if (getPlayer game player).piecePlaced < maxPiecesPerPlayer
        then
          let newGameUpdate = playRandomly seed player game Placing in
          play newGameUpdate (reverseColor player)
        else if (getPlayer game player).nbPiecesOnBoard <= 3
        then
          if lost game (getPlayer game player) false Flying
          then
            let () = afficheVainqueur (reverseColor player) in
            game
          else
            let newGameUpdate = playRandomly seed player game Flying in
            play newGameUpdate (reverseColor player)
        else if lost game (getPlayer game player) false Moving
        then
          let () = afficheVainqueur (reverseColor player) in
          game
        else
          let newGameUpdate = playRandomly seed player game Moving in
          play newGameUpdate (reverseColor player)
    in
    let board = gameboard in
    let p1, p2 = (initPlayer Black, initPlayer White) in
    let game = { board; mill = false; player1 = p1; player2 = p2; gameIsChanged = false } in
    play game Black
