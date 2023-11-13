exception Not_Allowed of string

exception Invalid_Strategy of string

val init_player_with_strategie :
  (Type.game_update -> Type.player -> Type.move) ->
  (Type.game_update -> Type.player -> Type.coordinates) ->
  Type.player_strategie

val arena : Type.player_strategie -> Type.player_strategie -> Board.template -> Type.game_update
