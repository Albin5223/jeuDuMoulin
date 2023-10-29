open Type

(** This represent the size of the board **)
let board_size = 13

(** Represent the maximum amount of pieces that each player can put on board *)
let maxPiecesPerPlayer = 9

(** Represent the number of pieces that you have to align to get a mill *)
let nbToGetMill = 3


let notUpdatedGame game = {board = game.board; mill = false; player1 = game.player1; player2 = game.player2; gameIsChanged = false}

(** Function that print a board square *)
let printSquare (s : square) = 
  match s with
  | Color(White) -> Format.printf "{W}"
  | Color(Black) -> Format.printf "{B}"
  | Empty -> Format.printf "{ }"
  | Path(H) -> Format.printf "---"
  | Path(V) -> Format.printf " | "
  | Path(DR) -> Format.printf " / "
  | Path(DL) -> Format.printf " \\ "
  | _ -> Format.printf " # "

let printMove (m : directionDeplacement) = 
  match m with
  | Up -> print_string "Up\n"
  | Down -> print_string "Down\n"
  | Right -> print_string "Right\n"
  | Left -> print_string "Left\n"
  | Up_right -> print_string "Up_right\n"
  | Up_left -> print_string "Up_left\n"
  | Down_right -> print_string "Down_right\n"
  | Down_left -> print_string "Down_left\n"

let prettyPrintListDirection l =
  l|>List.iter(fun a -> printMove a )

(** Returns the coordinates from some coordinates and its direction *)
let coordinatesFromDirections d (i,j) =
  match d with
  | Up -> (i-1,j)
  | Down -> (i+1,j)
  | Right -> (i,j+1)
  | Left -> (i,j-1)
  | Up_right -> (i-1,j+1)
  | Up_left -> (i-1,j-1)
  | Down_right -> (i+1,j+1)
  | Down_left -> (i+1,j-1)

let pathToHaveFromDirection d =
  match d with
  | Up | Down -> Path(V)
  | Right | Left -> Path(H)
  | Up_right | Down_left -> Path(DR)
  | Up_left | Down_right -> Path(DL)

let getSquare board (i,j) = 
  if i >= board_size || i < 0 || j >= board_size || j < 0 then None
  else Some (List.nth (List.nth board (i)) j)

(* TODO : Return an option *)
(** Function that return the row "i" of the board *)
let getRow (board:board) (i:int) :square list =
  List.nth board i

(* TODO : Return an option *)
(** Function that return the column "j" of the board *)
let getColumn (board:board) (j:int) :square list=
  List.fold_right (fun l acc -> [(List.nth l j)] @acc) board []

let coordinateFromDirection (board : board) ((i,j) : coordinates) (d : directionDeplacement) : coordinates option =
  let rec goTo (board : board) ((x,y):coordinates) (d : directionDeplacement) : coordinates option =
    let cooBis  = coordinatesFromDirections d (x,y) in
    let case = getSquare board cooBis in 
    if case = Some (pathToHaveFromDirection d)
      then goTo board cooBis d
    else 
      match case with
      | Some Empty | Some (Color _) -> Some cooBis
      | _ -> None
  in if getSquare board (coordinatesFromDirections d (i,j)) = Some (pathToHaveFromDirection d) then goTo board (i,j) d else None

(** Function that check if there is a mill from a certain position(i,j) *)
let checkMillFromPosition (board:board) ((i,j):coordinates) (color:color) : bool = 
  match getSquare board (i,j) with
  | Some (Color(c)) when c = color -> 
    let rec countFromD (x,y) d =
      match coordinateFromDirection board (x,y) d with
      | Some (a,b) -> if getSquare board (a,b) = Some (Color(color)) then 1 + countFromD (a,b) d else 0
      | _ -> 0
    in
    let count_row = (countFromD (i,j) Right) + (countFromD (i,j) Left) in
    let count_col = (countFromD (i,j) Up) + (countFromD (i,j) Down) in
    let count_diag1 = (countFromD (i,j) Up_right) + (countFromD (i,j) Down_left) in
    let count_diag2 = (countFromD (i,j) Up_left) + (countFromD (i,j) Down_right) in
    1 + max (max count_row count_col) (max count_diag1 count_diag2) >= nbToGetMill
  | _ -> false


(** A map that apply the function "f" to the square at the coordinate (i,j) of the board *)
let boardMap (f:square -> square) (board:board) ((i,j):coordinates) =
  List.mapi (fun x line -> if x = i then (List.mapi (fun y el -> if y = j then f el else el) line) else line) board

(**
  Function that put a piece on the board at the coordinate (i,j) and return the new board
  If the position is not legit for a piece, return the old state of the game with the old board
*)
let placePieceOnBoard (board : board) ((i,j):coordinates) color : gotMill = 
  let board = boardMap (fun x -> if x = Empty then Color(color) else x) board (i,j) in
  let check = checkMillFromPosition board (i,j) color in (board,check)

(** Function that put a start piece on the board at the coordinate (i,j) and return the new game state *)
let placeStartPiece (game : gameUpdate) ((i,j):coordinates) (color:color) : gameUpdate = 
  let concernedPlayer = if game.player1.color = color then game.player1 else game.player2 in
  if(getSquare game.board (i,j) = Some Empty && (concernedPlayer.piecePlaced < maxPiecesPerPlayer))
    then
      let (board, isMill) = placePieceOnBoard game.board (i,j) color in
      let updatedPlayer = {color = concernedPlayer.color; piecePlaced = concernedPlayer.piecePlaced + 1; nbPiecesOnBoard = concernedPlayer.nbPiecesOnBoard + 1; bag = concernedPlayer.bag@[(i,j)]} in
      if color = game.player1.color
        then {board = board;mill = isMill;player1 = updatedPlayer;player2 = game.player2;gameIsChanged=true}
        else {board = board;mill = isMill;player1 = game.player1;player2 = updatedPlayer;gameIsChanged=true}
    else
    {board = game.board;mill = false;player1 = game.player1;player2 = game.player2;gameIsChanged=false}

(**
  This function remove a piece from the board and returns it
  Return an unchanged board if there is no piece in (i,j) 
*)
let removeFromBoard (board : board) ((i,j) : coordinates) color : board = 
  boardMap (fun x -> if x = Color(color) then Empty else x) board (i,j)

(** This function eliminate a piece from the board and returns the new game state *)
let eliminatePiece (game : gameUpdate) (i,j) color : gameUpdate =
  if (getSquare game.board (i,j) != Some (Color color))  (* If the piece does not exist in (i,j), we do nothing *)
    then notUpdatedGame game
    else  (* Else, we remove the piece and apply the changes for the bag of the concerned player *)
      let concernedPlayer = if game.player1.color = color then game.player1 else game.player2 in
      let newBag = (List.filter (fun (x,y) -> (x,y) <> (i,j)) concernedPlayer.bag) in
      let newBoard = removeFromBoard game.board (i,j) color in
      let updatedPlayer = {color = concernedPlayer.color; piecePlaced = concernedPlayer.piecePlaced; nbPiecesOnBoard = concernedPlayer.nbPiecesOnBoard - 1; bag = newBag} in
      if color = game.player1.color
        then {board = newBoard;mill = false; player1 = updatedPlayer;player2 = game.player2;gameIsChanged=true}
        else {board = newBoard;mill = false; player1 = game.player1;player2 = updatedPlayer;gameIsChanged=true}

(**
  This function moves a piece from (i1,j1) to (i2,j2)
  Return the changed board if the move is legal, else, return the unchanged board
*)
let moveToCoordinates (game : gameUpdate) ((i1,j1):coordinates) ((i2,j2):coordinates) (color:color) : gameUpdate = 
  let arrive = getSquare game.board (i2,j2) in
  let depart = getSquare game.board (i1,j1) in
  if arrive = Some Empty && depart = Some (Color(color))
  then
    let concernedPlayer = if game.player1.color = color then game.player1 else game.player2 in
    let sub = removeFromBoard game.board (i1,j1) concernedPlayer.color in 
    let newBag = (List.map (fun (x,y) -> if (x,y) = (i1,j1) then (i2,j2) else (x,y)) concernedPlayer.bag) in
    let (newBoard,isMill) = placePieceOnBoard sub (i2,j2) concernedPlayer.color in
    let newPlayer = {color = concernedPlayer.color; piecePlaced = concernedPlayer.piecePlaced; nbPiecesOnBoard = concernedPlayer.nbPiecesOnBoard; bag = newBag} in
    if color = game.player1.color
      then {board = newBoard;mill = isMill; player1 = newPlayer; player2 = game.player2;gameIsChanged = true}
      else {board = newBoard;mill = isMill; player1 = game.player1; player2 = newPlayer;gameIsChanged = true}
  else notUpdatedGame game 

(** Print the board in the shell *)
let prettyPrintBoard (b : board) : unit = (List.iter (fun l -> List.iter (printSquare) l; Format.printf "@.") b) ; print_endline ""

(** Init a start board *)
let initBoard =  
  
  let rec aux x =
    let rec aux2 i j =
      match (i,j) with
      |(7,_)|(_,7)->[]
      |(3,3)->[Wall]@(aux2 i (j+1))
      |(3,_)|(_,3) -> [Empty]@(aux2 i (j+1))
      |(a,b) when (a = b || (a+b) = 6) ->[Empty]@(aux2 i (j+1))
      |(0,_)|(6,_)|(_,2)|(_,4)-> [Path(H)]@(aux2 i (j+1))
      |_-> [Path(V)]@(aux2 i (j+1))
      
 
      in
    if x < 7 then (aux2 x 0)::(aux (x+1)) else []  
  in 
  aux 0

(** Function that init a board with only the up-left quarter of it *)
let initBoardQuarter (quarter : board) : board =
  let reverseDiagonal = (fun el -> match el with | Path(DR) -> Path(DL) | Path(DL) -> Path(DR) | _ -> el) in
  let rec fullBoard (b : board) (newBoard : board) =
    let rec halfRow (row : square list) (newRow : square list) =
      match row with
      | [] -> []
      | x::[] -> newRow@[x]@(List.rev (List.map reverseDiagonal newRow))
      | x::xs -> halfRow xs (newRow@[x])
    in
    match b with
    | [] -> []
    | row::[] -> (newBoard@[(halfRow row [])]@(List.rev (List.map (fun line -> if (List.exists (fun x -> match x with | Path(DR) | Path(DL) -> true | _ -> false) line) then List.map reverseDiagonal line else line) newBoard)))
    | row::rs -> fullBoard rs (newBoard@[halfRow row []])
  in fullBoard quarter []

(** Function that move a piece from the coordinate (i,j) to a certain direction only if there is a Path in this direction *)
let moveToDirection (game : gameUpdate) ((i,j) : coordinates) (d : directionDeplacement) (color:color) : gameUpdate = 
  let rec goTo (game : gameUpdate) ((x,y):coordinates) (d : directionDeplacement) (color:color) : gameUpdate =
    let cooBis dir = coordinatesFromDirections dir (x,y) in
    let case = getSquare game.board (cooBis d) in if case = Some (pathToHaveFromDirection d) then goTo game (cooBis d) d color else (if case = (Some Empty) then moveToCoordinates game (i,j) (cooBis d) color else notUpdatedGame game)
  in if getSquare game.board (coordinatesFromDirections d (i,j)) = Some (pathToHaveFromDirection d) then goTo game (i,j) d color else notUpdatedGame game


let possibleMoves (game : gameUpdate) ((i,j) : coordinates) (player : color) (diagonal : bool): directionDeplacement list = 
  let rec aux (game : gameUpdate) ((i,j) : coordinates) (dir : directionDeplacement) : directionDeplacement list =
    if i >= board_size || i < 0 || j >= board_size || j < 0 then []
    else 
      match (List.nth (List.nth game.board i) j) with
      | Empty -> [dir]
      | Path(_) -> aux game (coordinatesFromDirections dir (i,j)) dir
      | _ -> []
  in
  match getSquare game.board (i,j) with
  | Some (Color(p)) when p = player ->
    let normalMoves = (aux game (i,j+1) Right)@(aux game (i,j-1) Left)@(aux game (i-1,j) Up)@(aux game (i+1,j) Down) in
    if diagonal then normalMoves@(aux game (i+1,j+1) Down_right)@(aux game (i-1,j+1) Up_right)@(aux game (i+1,j-1) Down_left)@(aux game (i-1,j-1) Up_left)
    else normalMoves
  | _ -> []