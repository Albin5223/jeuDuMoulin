exception Not_Allowed of string
val init_player :
  (Type.gameUpdate -> Type.move) ->
  (Type.gameUpdate -> Type.coordinates) -> Type.playerStrategie
val arena : Type.playerStrategie -> Type.playerStrategie -> Type.gameUpdate
