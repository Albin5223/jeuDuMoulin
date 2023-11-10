val init_player : Type.color -> Type.player

val max_pieces : int

val cant_move : Type.player -> Type.game_update -> bool

val play_randomly : (int -> int) -> Type.color -> Type.game_update -> Type.phase -> Type.game_update

val lost : Type.game_update -> Type.player -> bool
