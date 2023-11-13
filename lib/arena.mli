exception Not_Allowed of string

exception Invalid_Strategy of string

val init_player_with_strategie :
  (Type.game_update -> Type.player -> Type.move) ->
  (Type.game_update -> Type.player -> Type.coordinates) ->
  Type.player_strategie

val arena : Type.player_strategie -> Type.player_strategie -> Type.template -> Type.game_update

val player_random : (int -> int) -> Type.player_strategie
