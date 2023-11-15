exception Not_Allowed of string

exception Invalid_Strategy of string

val init_player_with_strategie :
  (Type.game_update -> Type.player -> Type.action) ->
  (Type.game_update -> Type.player -> Type.action) ->
  Type.player_strategie

val private_play : Type.game_update -> Type.player_strategie -> Type.player -> Type.player -> Type.game_update

val arena : Type.player_strategie -> Type.player_strategie -> Type.template -> Type.end_game

val player_random : (int -> int) -> Type.player_strategie

val read : string -> int

val give_direction : Type.direction_deplacement -> Type.direction_deplacement list -> string

val affiche_dir : Type.coordinates -> Type.game_update -> Type.color -> unit

val player_human : Type.player_strategie
