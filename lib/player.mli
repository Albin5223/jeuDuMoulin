val initPlayer : Type.color -> Type.player
val max_pieces : int
val reverseColor : Type.color -> Type.color
val cantMove : Type.player -> Type.gameUpdate -> bool -> bool
val playRandomly :
  (int -> int) ->
  Type.player ->
  Type.player -> Type.gameUpdate -> Type.phase -> Type.gameUpdate
val lost : Type.gameUpdate -> Type.player -> bool -> Type.phase -> bool
