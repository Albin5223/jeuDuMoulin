val init_player : Type.color -> Type.player

val cant_move : Type.player -> Type.game_update -> bool

val player_random : (int -> int) -> Type.player_strategie

val lost : Type.game_update -> Type.player -> bool

val determine_best_move : Type.game_update -> int -> Type.move