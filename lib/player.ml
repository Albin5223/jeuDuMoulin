open Type;;
open Board;;

type node = {value : int; move : move; player1 : player; player2 : player}

type tree =
    | Leaf
    | Nodes of node * tree list

let getNode (tree : tree) : node =
    match tree with
    | Leaf -> raise (Invalid_argument "getNode with an Empty tree !")
    | Nodes(node,_) -> node

let getChilds (tree : tree) : tree list =
    match tree with
    | Leaf -> raise (Invalid_argument "getChilds with an Empty tree !")
    | Nodes(_,childs) -> childs

(**merge a tree with his child and return a new tree*)
let merge (treeFather : tree) (treeChild : tree) : tree =
    match treeFather with
    | Leaf -> raise (Invalid_argument "merge with a leaf as a parent !")
    | Nodes(node,childs) -> Nodes(node,childs@[treeChild])

let mergeSiblings (treeFirstBorn : tree) (treeYounger : tree) : tree =
    match treeFirstBorn with
    | Leaf -> raise (Invalid_argument "merge with a leaf as a sibling !")
    | Nodes(node,childs) -> Nodes(node,childs@(getChilds treeYounger))

(*
let print_node (node : node) : unit = 
    Format.printf "{value=%d;move=" node.value;
    pretty_print_move node.move;
    Format.printf ";player1=";
    pretty_print_player node.player1;
    Format.printf ";player2=";
    pretty_print_player node.player2;
    Format.printf "}@.";;
*)

(**
    This function init a player based on a color
    @param c : the color of the player   
*)
let init_player (c : Type.color) : player =
    {phase = Placing; color = c; bag = []; piece_placed = 0; nb_pieces_on_board = 0}

(**
    This function return a bool if the player can't move
    @param player : the player
    @param game : the game
*)
let cant_move (player : player) (game : game_update) : bool =
    let rec aux (player : player) (game : game_update) (bag : coordinates list) : bool =
        match bag with
        | [] -> true
        | (x, y) :: xs -> List.length (possible_moves game (x, y) player.color) = 0 && aux player game xs
    in
    aux player game player.bag

(**
    Player who plays ramdomly
    @param random : the seed of the random
*)
let player_random (random : int -> int) : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : move =
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
    let strategie_remove (game_update : game_update) (player : player) : move =
        let i = random (List.length (get_opponent game_update player.color).bag) in
        Removing(List.nth (get_opponent game_update player.color).bag i)
    in
    {strategie_play; strategie_remove}

(**
    Function that return a bool if the player lost
    @param game : the game
    @param player : the player    
*)
let lost (game : game_update) (player : player) : bool =
    match player.phase with
    | Moving -> cant_move player game
    | _ -> player.nb_pieces_on_board <= 2 && player.piece_placed = game.max_pieces


(*
----------------------------------------------------------------------------
AI PART :


(it is admitted that the current bot we working on is always white)

*)

(**protype of an evalutation function for the AI*)
let rate_value (node : node) : int = 
    match node.player1.color with
    | Black -> node.player1.nb_pieces_on_board - node.player2.nb_pieces_on_board
    | White -> node.player2.nb_pieces_on_board - node.player1.nb_pieces_on_board

(** return the minimum value of all the childs of a tree*)
let minimax (tree : tree) : int =
    let rec aux (isMin : bool) (l : tree list) (minOrMax : float) : int =
        match l with
        | [] -> int_of_float minOrMax
        | tree :: xs -> (
            if isMin then aux isMin xs (min (float_of_int (getNode tree).value) minOrMax)
            else aux isMin xs (max (float_of_int (getNode tree).value) minOrMax)
        )
    in
    match tree with
    | Leaf -> raise (Failure "Big problem dude this is not supposed to happen")
    | Nodes(node,_) -> (
        if (node.player1.color = Black) then
            aux true (getChilds tree) max_float
        else aux false (getChilds tree) min_float
    )

(**private function only use is in determine_best_move.
    This function create the tree we'll be working on and tests every possible move of the bot and of the opponent recursively*)
let rec create_next_nodes (depth : int) (accTree : tree) (gameUpdate : game_update) : tree =
    if depth = 0 then accTree
    else (
        (*so if the player has to remove a marble*)
        if (gameUpdate.mill) then (
            let rec loop (bagP : coordinates list) (accTreeFinal : tree) =
                match bagP with
                | [] -> accTreeFinal
                | (x,y) :: xs -> (
                    let gameUpdate2 = eliminate_piece (gameUpdate) (x,y) (gameUpdate.player2.color) in
                    if (depth = 1 || lost (gameUpdate2) (gameUpdate2.player2)) then (
                        let child = create_next_nodes (depth-1) (Nodes({value=rate_value (getNode accTree);move=Removing((x,y));player1=gameUpdate2.player2;player2=gameUpdate2.player1},[Leaf])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player2;player2=gameUpdate2.player1;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces}) in
                        loop (xs) (merge accTreeFinal child)
                    )
                    else (
                        let v = (minimax accTree) in
                        (*a voir si on doit pas merge accTree et le node qu'on fait avec gameUpdate2 avant de faire la recursion*)
                        let child = create_next_nodes (depth-1) (Nodes({value=v;move=Removing((x,y));player1=gameUpdate2.player2;player2=gameUpdate2.player1},[])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player2;player2=gameUpdate2.player1;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces}) in
                        loop (xs) (merge accTreeFinal child)
                    )
                )
            in
            loop (gameUpdate.player2.bag) accTree
            (*process every opponent marble because we need opponent's coordinates and not ours*)
        )

        else (

        match gameUpdate.player1.phase with

        (*Phase placing*)
        | Placing -> (
            let rec loop (board2 : board) (i : int) (accTreeFinal : tree) : tree =
                match board2 with
                | [] -> accTreeFinal
                | x :: xs -> (
                    let rec loop2 (row : square list) (j : int) (accTreeFinal2 : tree) : tree =
                        match row with
                        | [] -> accTreeFinal2
                        | Empty :: xs2 -> (
                            if depth = 1 then (
                                let gameUpdate2 = place_start_piece (gameUpdate) (i,j) gameUpdate.player1.color in
                                let child = (
                                    if gameUpdate2.mill then create_next_nodes (depth-1) (Nodes({value=rate_value (getNode accTree);move=Placing((i,j));player1=gameUpdate2.player1;player2=gameUpdate2.player2},[Leaf])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player1;player2=gameUpdate2.player2;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                    else create_next_nodes (depth-1) (Nodes({value=rate_value (getNode accTree);move=Placing((i,j));player1=gameUpdate2.player2;player2=gameUpdate2.player1},[Leaf])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player2;player2=gameUpdate2.player1;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                ) in
                                loop2 xs2 (j+1) (merge accTreeFinal2 child)
                            )
                            else (
                                let gameUpdate2 = place_start_piece (gameUpdate) (i,j) gameUpdate.player1.color in
                                let v = (minimax accTree) in
                                let child = (
                                    if gameUpdate2.mill then create_next_nodes (depth-1) (Nodes({value=v;move=Placing((i,j));player1=gameUpdate2.player1;player2=gameUpdate2.player2},[])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player1;player2=gameUpdate2.player2;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                    else create_next_nodes (depth-1) (Nodes({value=v;move=Placing((i,j));player1=gameUpdate2.player2;player2=gameUpdate2.player1},[])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player2;player2=gameUpdate2.player1;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                ) in
                                loop2 xs2 (j+1) (merge accTreeFinal2 child)
                            )
                        )
                        | _ :: xs2 -> loop2 xs2 (j+1) (accTreeFinal2)
            in
            let child = loop2 x 0 (accTreeFinal) in
            loop (xs) (i+1) (mergeSiblings accTree child)
            )
        in
        let tmp = loop gameUpdate.board 0 (accTree) in
        tmp
        )


        (*Phase moving*)
        | Moving -> (
            let rec loop (bagP : coordinates list) (accTreeFinal : tree) =
                match bagP with
                | [] -> accTreeFinal
                | (x,y) :: xs -> (
                    let rec loop2 (possiblesMoves : direction_deplacement list) (accTreeFinal2 : tree) =
                        match possiblesMoves with
                        | [] -> accTreeFinal2
                        | dir :: xs2 -> (
                            if depth = 1 then (
                                let (x2,y2) = Option.get (move_from_direction (gameUpdate.board) (x,y) (dir)) in
                                let gameUpdate2 = move_to_coordinates (gameUpdate) (x,y) (x2,y2) gameUpdate.player1.color in
                                let child = (
                                    if gameUpdate2.mill then create_next_nodes (depth-1) (Nodes({value=rate_value (getNode accTree);move=Moving((x,y),dir);player1=gameUpdate2.player1;player2=gameUpdate2.player2},[Leaf])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player1;player2=gameUpdate2.player2;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                    else create_next_nodes (depth-1) (Nodes({value=rate_value (getNode accTree);move=Moving((x,y),dir);player1=gameUpdate2.player2;player2=gameUpdate2.player1},[Leaf])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player2;player2=gameUpdate2.player1;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                ) in
                                loop2 xs2 (merge accTreeFinal2 child)
                            )
                            else (
                                let (x2,y2) = Option.get (move_from_direction (gameUpdate.board) (x,y) (dir)) in
                                let gameUpdate2 = move_to_coordinates (gameUpdate) (x,y) (x2,y2) gameUpdate.player1.color in
                                let v = (minimax accTree) in
                                let child = (
                                    if gameUpdate2.mill then create_next_nodes (depth-1) (Nodes({value=v;move=Moving((x,y),dir);player1=gameUpdate2.player1;player2=gameUpdate2.player2},[])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player1;player2=gameUpdate2.player2;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                    else create_next_nodes (depth-1) (Nodes({value=v;move=Moving((x,y),dir);player1=gameUpdate2.player2;player2=gameUpdate2.player1},[])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player2;player2=gameUpdate2.player1;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                ) in
                                loop2 xs2 (merge accTreeFinal2 child)
                            )
                        )
                    in
                    let child = loop2 (possible_moves gameUpdate (x,y) (gameUpdate.player1.color)) (accTreeFinal) in
                    loop xs (mergeSiblings accTree child)
                )
            in
            loop gameUpdate.player1.bag accTree
        )


        (*so if the current phase is Flying*)
        | Flying -> (
            let rec loop (bagP : coordinates list) (accTreeFinal : tree) =
                match bagP with
                | [] -> accTreeFinal
                | (x,y) :: xs-> (
                    let rec loop2 (board2 : board) (i : int) (accTreeFinal2 : tree) =
                        match board2 with
                        | [] -> accTreeFinal2
                        | l :: xs2 -> (
                            let rec loop3 (row : square list) (j : int) (accTreeFinal3 : tree) =
                                match row with
                                | [] -> accTreeFinal3
                                | Empty :: xs3 -> (
                                    if (depth = 1) then (
                                        let gameUpdate2 = move_to_coordinates (gameUpdate) ((x,y)) ((i,j)) (gameUpdate.player1.color) in
                                        let child = (
                                            if gameUpdate2.mill then create_next_nodes (depth-1) (Nodes({value=rate_value (getNode accTree);move=Flying((x,y),(i,j));player1=gameUpdate2.player1;player2=gameUpdate2.player2},[Leaf])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player1;player2=gameUpdate2.player2;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                            else create_next_nodes (depth-1) (Nodes({value=rate_value (getNode accTree);move=Flying((x,y),(i,j));player1=gameUpdate2.player2;player2=gameUpdate2.player1},[Leaf])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player2;player2=gameUpdate2.player1;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                        ) in
                                        loop3 (xs3) (j+1) (merge accTreeFinal3 child)
                                    )
                                    else (
                                        let gameUpdate2 = move_to_coordinates (gameUpdate) ((x,y)) ((i,j)) (gameUpdate.player1.color) in
                                        let v = (minimax accTree) in
                                        let child = (
                                            if gameUpdate2.mill then create_next_nodes (depth-1) (Nodes({value=v;move=Flying((x,y),(i,j));player1=gameUpdate2.player1;player2=gameUpdate2.player2},[])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player1;player2=gameUpdate2.player2;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                            else create_next_nodes (depth-1) (Nodes({value=v;move=Flying((x,y),(i,j));player1=gameUpdate2.player2;player2=gameUpdate2.player1},[])) ({board=gameUpdate2.board;mill=gameUpdate2.mill;player1=gameUpdate2.player2;player2=gameUpdate2.player1;game_is_changed=gameUpdate2.game_is_changed;max_pieces=gameUpdate2.max_pieces})
                                        ) in
                                        loop3 (xs3) (j+1) (merge accTreeFinal3 child)
                                    )
                                )
                                | _ :: xs3 -> loop3 (xs3) (j+1) (accTreeFinal3)
                            in
                            let child = loop3 (l) (0) (accTreeFinal2) in
                            loop2 (xs2) (i+1) (mergeSiblings accTree child)
                        )
                    in
                    let child = loop2 (gameUpdate.board) (0) (accTreeFinal) in
                    loop (xs) (mergeSiblings accTree child)
                )
            in
            loop (gameUpdate.player1.bag) (accTree)
        )
        )
    )

let determine_best_move (depth : int) (gameUpdate : game_update) (player1 : player) : move =
    let tree = create_next_nodes depth (Nodes({value=0;move=Placing((0,0));player1=player1;player2=gameUpdate.player2},[])) (gameUpdate) in
    let rec aux (l : tree list) (maxValue : int) (maxNode : node) : node =
        match l with
        | [] -> maxNode
        | tree :: xs -> (
            if (getNode tree).value > maxValue then aux xs ((getNode tree).value) (getNode tree)
            else aux xs maxValue maxNode
        )
    in
    let bestNode = aux (getChilds tree) (min_int) ({value=min_int;move=Placing((0,0));player1=gameUpdate.player1;player2=gameUpdate.player2}) in
    (*Format.printf "@.bestMove selected is :@.move=";
    pretty_print_move (bestNode.move);
    Format.printf "with a value of %d@." bestNode.value;*)
    bestNode.move
