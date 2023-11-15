exception Not_Allowed of string

val init_player_with_strategie :
  (Type.game_update -> Type.player -> Type.move) ->
  (Type.game_update -> Type.player -> Type.move) ->
  Type.player_strategie

val privatePlay : Type.game_update -> Type.player_strategie -> Type.player -> Type.player -> Type.game_update

val update_player_phase : Type.player -> int -> Type.player

val update_phase : Type.game_update -> Type.game_update

val arena : Type.player_strategie -> Type.player_strategie -> Board.template -> Type.game_update
