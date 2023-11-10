val initPlayer : Type.color -> Type.player

val max_pieces : int

val cantMove : Type.player -> Type.gameUpdate -> bool -> bool

val playRandomly : (int -> int) -> Type.color -> Type.gameUpdate -> Type.phase -> Type.gameUpdate

val lost : Type.gameUpdate -> Type.player -> bool -> Type.phase -> bool
