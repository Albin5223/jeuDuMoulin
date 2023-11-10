exception Not_Allowed of string

val init_player_with_strategie :
  (Type.game_update -> Type.color -> Type.move) ->
  (Type.game_update -> Type.color -> Type.coordinates) ->
  Type.player_strategie

val arena : Type.player_strategie -> Type.player_strategie -> Type.game_update
