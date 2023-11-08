open Type;;
open Board;;
open Player;;

let quarter =
  [
    [Empty; Path(H); Path(H); Path(H); Path(H); Path(H); Empty];
    [Path(V); Wall; Wall; Wall; Wall; Wall; Path(V)];
    [Path(V); Wall; Empty; Path(H); Path(H);Path(H);Empty];
    [Path(V); Wall; Path(V); Wall; Wall; Wall; Path(V)];
    [Path(V); Wall; Path(V); Wall; Empty; Path(H); Empty];
    [Path(V); Wall; Path(V); Wall; Path(V); Wall; Wall];
    [Empty; Path(H); Empty; Path(H); Empty; Wall; Wall]
  ]

let gameboard = initBoardQuarter quarter

let player_turn (diag : bool) =
  let rec play (game :gameUpdate) (player:color):gameUpdate =

    let rec deplace = 

      let affiche_dir  ((i,j) : coordinates) (player : color) (diagonal : bool) = 
        let aux (dir : directionDeplacement) (l : directionDeplacement list) = 
          try  let x = List.find (fun x -> x = dir) l 
          in 
            match x with
              |Up_left -> " 1 "
              |Up -> "2 "
              |Up_right -> " 3"
              |Left -> "4  "
              |Right -> " 5"
              |Down_left -> "6 "
              |Down -> " 7 "
              |_-> " 8"
        
          with Not_found -> "  "
        in
          let deplacements = possibleMoves game  (i,j) player diagonal 
          in
            let l1 = aux Up_left  deplacements ^ aux Up  deplacements ^ aux Up_right deplacements
            in let l2 = aux Left  deplacements ^ " x " ^ aux Right deplacements
            in let l3 = aux Down_left  deplacements ^ aux Down  deplacements ^ aux Down_right deplacements 
            in print_endline l1;print_endline l2; print_endline l3

      in

      let  ()  = print_string  "in which line is the mem you want to move ? (i) : " in 
      let  i  = read_int  () in 
      let  ()  = print_string  "in which column is the mem you want to move ? : (j) " in 
      let j = read_int () in
      let () = affiche_dir (i,j) player diag in 
      let  ()  = print_string  "what direction do you choose ? : " in
      let  intDir= read_int () in

      try 
      let directionDeplacement = (
      match intDir with
      |1 -> Up_left
      |2 -> Up
      |3 -> Up_right
      |4 -> Left
      |5 -> Right
      |6 -> Down_left
      |7 -> Down
      |8 -> Down_right
      |_ -> raise Not_found ) in


      let gamechanged = moveToDirection game (i,j) directionDeplacement player in

      if not (gamechanged.gameIsChanged ) then let () =  print_endline "mouvement impossible " in play game  player
      
      else

      if (getPlayer game player).nbPiecesOnBoard <= 3
        then if lost game (getPlayer game player) false (Flying(player)) then let () = afficheVainqueur (reverseColor player) in game 
          else play gamechanged (reverseColor player)
      else
        if lost game (getPlayer game player) false Moving then let () = afficheVainqueur (reverseColor player) in game
        else play gamechanged (reverseColor player)  

      with Not_found ->let () =  print_endline "mouvement impossible " in play game  player  

    in

    let () = prettyPrintBoard game.board in
      if not ((getPlayer game player).piecePlaced<maxPiecesPerPlayer)
        then  let  ()  = print_string  "to creat a new men, press 1, else press 0 : " in 
                let  a  = read_int  () in
                  if (a = 1) (*create a new mem*) 
                    then let  ()  = print_string  "in which line do you want to place it ? (i) : " in 
                      let  i  = read_int  () in 
                      let  ()  = print_string  "in which column do you want to place it ? : (j) " in 
                      let j = read_int () in
                      placeStartPiece game (i,j) player
                        
                    else (*move a existing mem*)
                    deplace 

                  
        else
           deplace 
  in let board = gameboard in let (p1,p2) = (initPlayer Black,initPlayer White) in 
  let game = {board = board;mill=false;player1 = p1;player2=p2;gameIsChanged=false} in 
  play game White

            

let gameRandomly seed =
  let rec play (game :gameUpdate) (player:color):gameUpdate =
    let () = prettyPrintBoard game.board in
    if (getPlayer game player).piecePlaced<maxPiecesPerPlayer
      then  let newGameUpdate = playRandomly seed player game Placing in play newGameUpdate (reverseColor player)
    else

      if (getPlayer game player).nbPiecesOnBoard <= 3
        then if lost game (getPlayer game player) false (Flying(player)) then let () = afficheVainqueur (reverseColor player) in game 
          else let newGameUpdate = playRandomly seed player game BothFlying  in play newGameUpdate (reverseColor player)
      else
        if lost game (getPlayer game player) false Moving then let () = afficheVainqueur (reverseColor player) in game
        else let newGameUpdate = playRandomly seed player game Moving  
      in play newGameUpdate (reverseColor player)
  in let board = gameboard in let (p1,p2) = (initPlayer Black,initPlayer White) in 
  let game = {board = board;mill=false;player1 = p1;player2=p2;gameIsChanged=false} in 
  play game Black
      