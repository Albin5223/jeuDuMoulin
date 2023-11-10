val init_player : Type.color -> Type.player

val max_pieces : int

val cant_move : Type.player -> Type.game_update -> bool

val player_randomly : (int -> int) -> Type.player_strategie
