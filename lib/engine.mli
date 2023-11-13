val nb_to_get_mill : int

val get_player : Type.game_update -> Type.color -> Type.player

val reverse_color : Type.color -> Type.color

val get_opponent : Type.game_update -> Type.color -> Type.player

val coordinates_from_directions : Type.direction_deplacement -> int * int -> int * int

val get_square : Type.board -> int * int -> Type.square option

val get_row : Type.board -> int -> Type.square list

val get_column : Type.board -> int -> Type.square list

val node_from_direction : Type.board -> Type.coordinates -> Type.direction_deplacement -> Type.coordinates option

val check_mill_from_position : Type.board -> Type.coordinates -> Type.color -> bool

val place_piece_on_board : Type.board -> Type.coordinates -> Type.color -> Type.got_mill

val place_start_piece : Type.game_update -> Type.coordinates -> Type.color -> Type.game_update

val eliminate_piece : Type.game_update -> Type.coordinates -> Type.color -> Type.game_update

val move_to_coordinates : Type.game_update -> Type.coordinates -> Type.coordinates -> Type.color -> Type.game_update

val move_to_direction :
  Type.game_update -> Type.coordinates -> Type.direction_deplacement -> Type.color -> Type.game_update

val possible_moves : Type.game_update -> Type.coordinates -> Type.color -> Type.direction_deplacement list

val apply : Type.game_update -> Type.player -> Type.move -> Type.game_update

val max_piece_from_template : Type.template -> int

val init_board_with_template : Type.template -> Type.board

val cant_move : Type.player -> Type.game_update -> bool

val lost : Type.game_update -> Type.player -> bool

val init_player : Type.color -> Type.player

val update_phase : Type.game_update -> Type.game_update
