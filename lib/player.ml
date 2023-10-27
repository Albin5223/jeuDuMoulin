open Type;;
open Board;;

let initPlayer (c : Type.color) : player = {color = c; bag = []; piecePlaced = 0; nbPiecesOnBoard = 0};;

(** The max amount of pieces that a player can hold *)
let max_pieces = 9

let reverseColor (c : Type.color) : Type.color =
  match c with
  | Type.Black -> Type.White
  | Type.White -> Type.Black

let cantMove (player : player) (game : gameUpdate) (diagonal : bool) : bool =
  let rec aux (player : player) (game : gameUpdate) (bag : coordinates list) (diagonal : bool) : bool =
    match bag with
    | [] -> true
    | (x,y) :: xs -> List.length (possibleMoves game (x,y) player.color diagonal) = 0 && (aux player game xs diagonal)
  in
  aux player game player.bag diagonal

let rec playRandomly (random) (player : player) (opponent : player) (game : gameUpdate) (current_phase : phase) : gameUpdate =
  if current_phase = Placing then 
    let i = random board_size in
    let j =  random board_size in
    let tmp = placeStartPiece game (i,j) player.color in
    if tmp.mill
      then eliminatePiece game (List.nth (opponent.bag) (random opponent.nbPiecesOnBoard)) opponent.color
    else if not tmp.gameIsChanged then playRandomly random player opponent game current_phase (*if we choose coordinates where a piece is already here or a path or a wall*)
    else tmp

  else if current_phase = Moving || current_phase = Flying(opponent.color) then (*either the bot can just move or the opponent is flying but not the bot*)
    let (x,y) = (List.nth player.bag (random player.nbPiecesOnBoard)) in
    let movesPossible = possibleMoves game (x,y) player.color false in
    if List.length movesPossible = 0 then playRandomly random player opponent game current_phase (*if we have a unmovable piece, we examine another piece*)
    else
      let tmp = moveToDirection game (x,y) (List.nth movesPossible (random (List.length movesPossible))) player.color in
      if tmp.mill 
        then eliminatePiece game (List.nth (opponent.bag) (random opponent.nbPiecesOnBoard)) opponent.color
      else tmp

  else (*this means either the bot is flying or both players are flying*)
    let i = random board_size in
    let j = random board_size in
    let tmp = moveToCoordinates game ((List.nth (player.bag) (random player.nbPiecesOnBoard))) (i,j) player.color in
    if tmp.mill then
      eliminatePiece game (List.nth (opponent.bag) (random opponent.nbPiecesOnBoard)) opponent.color
    else if not tmp.gameIsChanged then playRandomly random player opponent game current_phase (*if we choose coordinates inside a wall or a path*)
    else tmp

let lost (game : gameUpdate) (player : player) (diagonal : bool) (current_phase : phase) : bool =
  ((current_phase = Moving || current_phase = Flying(reverseColor player.color)) && cantMove player game diagonal) || player.nbPiecesOnBoard <= 2