open Type

(** This represent the size of the board **)
let board_size = 7

(** Represent the maximum amount of pieces that each player can put on board *)
let maxPiecesPerPlayer = 9


let notUpdatedGame game = {board = game.board; mill = game.mill; player1 = game.player1; player2 = game.player2; gameIsChanged = false}

(** Function that print a board square *)
let printSquare (s : square) = 
  match s with
  | Color(White) -> Format.printf "{W}"
  | Color(Black) -> Format.printf "{B}"
  | Empty -> Format.printf "{ }"
  | Path(H) -> Format.printf "---"
  | Path(V) -> Format.printf " | "
  |_ -> Format.printf "   "

let printMove (m : directionDeplacement) = 
  match m with
  | Up -> Format.printf "Up"
  | Down -> Format.printf "Down"
  | Right -> Format.printf "Right"
  | Left -> Format.printf "Left"
  | Up_right -> Format.printf "Up_right"
  | Up_left -> Format.printf "Up_left"
  | Down_right -> Format.printf "Down_right"
  | Down_left -> Format.printf "Down_left"

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

let getSquare board (i,j) = 
  if i >= board_size || i < 0 || j >= board_size || j < 0 then None
  else Some (List.nth (List.nth board (i)) j)

let getRow (board:board) (i:int) :square list =
  List.nth board i

let getColumn (board:board) (j:int) :square list=
  List.fold_right (fun l acc -> [(List.nth l j)] @acc) board []

let checkMillFromList subBoard color : int =
  List.fold_right (fun a b -> if a = Color(color) then b+1 else if a = Wall then 0 else b) subBoard 0

let checkMillInMid subBoard j color = 
  let rec aux subBoard distance count=
    match subBoard with 
    |[] -> count
    |Wall::_ when distance = 0-> count
    |x::xs when distance = 0 -> if x = Color(color) then aux xs distance (count+1) else aux xs distance count
    |_::xs -> aux xs (distance-1) count
  in if j<board_size/2 then aux subBoard 0 0 else aux subBoard 4 0


(**Function that check if there is a mill from a certain position(i,j)**)
  (*A FINIIIIR*)
let checkMillFromPosition (board:board) ((i,j):coordinates) color : bool = 
  match i,j with 
  |(3,_) -> (checkMillFromList (getColumn board j) color = 3) || (checkMillInMid (getRow board i) j color = 3)
  |(_,3) -> (checkMillFromList (getRow board i) color = 3) || (checkMillInMid (getColumn board j) i color = 3)
  |_ ->(checkMillFromList (getRow board i) color = 3) || (checkMillFromList (getColumn board j) color = 3)

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
  [[Empty;Path(H);Path(H);Empty;Path(H);Path(H);Empty];
[Path(V);Empty;Path(H);Empty;Path(H);Empty;Path(V)];
[Path(V);Path(V);Empty;Empty;Empty;Path(V);Path(V)];
[Empty;Empty;Empty;Wall;Empty;Empty;Empty];
[Path(V);Path(V);Empty;Empty;Empty;Path(V);Path(V)];
[Path(V);Empty;Path(H);Empty;Path(H);Empty;Path(V)];
[Empty;Path(H);Path(H);Empty;Path(H);Path(H);Empty]]



(** Function that move a piece from the coordinate (i,j) to a certain direction *)
let moveToDirection (game : gameUpdate) ((i,j) : coordinates) (d : directionDeplacement) (color:color) : gameUpdate =
  let cooBis dir = coordinatesFromDirections dir (i,j) in 
  let rec goTo (game : gameUpdate) _ (d : directionDeplacement) (color:color) : gameUpdate =
    let case = getSquare game.board (cooBis d ) in if case = Some (Path V) then goTo game (cooBis d) d color else (if case = Some Empty then moveToCoordinates game (i,j) (cooBis d) color else notUpdatedGame game)
  in goTo game (i,j) d color


let possibleMoves (game : gameUpdate) ((i,j) : coordinates) (player : color) (diagonal : bool): directionDeplacement list = 
  let aux (game : gameUpdate) ((i,j) : coordinates) (dir : directionDeplacement) : directionDeplacement list =
    if i >= board_size || i < 0 || j >= board_size || j < 0 then []
    else if List.nth (List.nth game.board i) j = Empty then [dir] else []
  in
  match getSquare game.board (i,j) with
  | Some (Color(p)) when p = player ->
    let normalMoves = (aux game (i,j+1) Right)@(aux game (i,j-1) Left)@(aux game (i-1,j) Up)@(aux game (i+1,j) Down) in
    if diagonal then normalMoves@(aux game (i+1,j+1) Down_right)@(aux game (i-1,j+1) Up_right)@(aux game (i+1,j-1) Down_left)@(aux game (i-1,j-1) Up_left)
    else normalMoves
  | _ -> []


(*Faire une IA qui joue au pif*)
(*Faire le changement de phase entre il peut placer où il veut et ensuite move de case en case*)
(*Faire des tests pour verifier les possibles erreurs*)

