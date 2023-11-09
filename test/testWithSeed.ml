open Mill.Type;;
open Mill.Game;;

let equalsBoard (board1: board) (board2:board) :bool =
  let rec compare l1 l2 = 
    match (l1,l2) with
    |([],[]) -> true
    |([],_)-> false
    |(_,[]) -> false
    |(x::xs,y::ys) ->
      match x,y with 
      |Empty,Empty -> compare xs ys
      |Wall,Wall -> compare xs ys
      |Path(d),Path(g) when d=g -> compare xs ys
      |Color(c1),Color(c2) when c1 = c2 -> compare xs ys
      |_-> false
  in compare (List.flatten board1) (List.flatten board2)


let equalsCoordinate (c1:coordinates) (c2:coordinates) :bool = 
  fst c1 = fst c2 && snd c1 = snd c2

let rec equalsListCoordinate (l1:coordinates list)(l2:coordinates list) :bool =
  match l1,l2 with
  |[],[] -> true
  |[],_ -> false
  |_,[] -> false
  |x::xs,y::ys -> equalsCoordinate x y && equalsListCoordinate xs ys

let equalsPlayer (p1:player)(p2:player):bool = 
  (p1.color = p2.color) && (equalsListCoordinate p1.bag p2.bag) && 
  (p1.piecePlaced = p2.piecePlaced) && (p1.nbPiecesOnBoard = p2.nbPiecesOnBoard)

let equalsGameUpdate (game1:gameUpdate)(game2:gameUpdate):bool = 
  (equalsBoard game1.board game2.board) && (game1.mill=game2.mill) && 
  (equalsPlayer game1.player1 game2.player1) && (equalsPlayer game1.player2 game2.player2) &&
  (game1.gameIsChanged = game2.gameIsChanged)
  



(*Faire en sorte que autour de chaque noeud, il n'y a pas de noeud*)

let testSeed =
  let open QCheck in
  Test.make ~count:100
    ~name:
      "for all seed : END gamePlayWithSeed = END gamePlayWithSeed when both seeds are the same"
      (small_int) 
      (fun x -> Random.init x;let randomSeed n = Random.int n in 
        let g1 = gameRandomly randomSeed in 
        let () = Random.init x in let randomSeed n = Random.int n in let g2 = gameRandomly randomSeed in 
        equalsGameUpdate g1 g2)
    
let () =
  let open Alcotest in
  run "SEED"
    [
      ("Test with Seed generate", [ QCheck_alcotest.to_alcotest testSeed ]);
    ]
