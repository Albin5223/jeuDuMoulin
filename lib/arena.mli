exception Not_Allowed of string

exception Invalid_Strategy of string

val init_player_with_strategie :
  (Type.game_update -> Type.player -> Type.action) ->
  (Type.game_update -> Type.player -> Type.action) ->
  Type.player_strategie

val arena : Type.player_strategie -> Type.player_strategie -> Type.template -> Type.end_game

val player_random : (int -> int) -> Type.player_strategie

val player_human : Type.player_strategie
