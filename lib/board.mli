val nb_to_get_mill : int

val not_updated_game : Type.game_update -> Type.game_update

val coordinates_from_directions : Type.direction_deplacement -> int * int -> int * int

val path_to_have_from_direction : Type.direction_deplacement -> Type.square

val get_square : Type.board -> int * int -> Type.square option

val get_square_row : Type.square list -> int -> Type.square option

val get_row : Type.board -> int -> Type.square list

val get_column : Type.board -> int -> Type.square list

val node_from_direction : Type.board -> Type.coordinates -> Type.direction_deplacement -> Type.coordinates option

val check_mill_from_position : Type.board -> Type.coordinates -> Type.color -> bool

val board_map : (Type.square -> Type.square) -> Type.board -> Type.coordinates -> Type.square list list

val place_piece_on_board : Type.board -> Type.coordinates -> Type.color -> Type.got_mill

val place_start_piece : Type.game_update -> Type.coordinates -> Type.color -> Type.game_update

val remove_from_board : Type.board -> Type.coordinates -> Type.color -> Type.board

val eliminate_piece : Type.game_update -> Type.coordinates -> Type.color -> Type.game_update

val move_to_coordinates : Type.game_update -> Type.coordinates -> Type.coordinates -> Type.color -> Type.game_update

val init_board : Type.square list list

val init_board_quarter : Type.board -> Type.board

val test_3_squares_row : Type.board -> int -> int -> int -> int -> int -> int -> bool

val aux_init_board : int -> int -> int -> int -> bool -> Type.board -> Type.board

val init_board2 : int -> int -> int -> bool -> Type.board

val move_to_direction :
  Type.game_update -> Type.coordinates -> Type.direction_deplacement -> Type.color -> Type.game_update

val possible_moves : Type.game_update -> Type.coordinates -> Type.color -> Type.direction_deplacement list

val apply : Type.game_update -> Type.player -> Type.move -> Type.game_update

type template = Three_mens_morris | Six_mens_morris | Nine_mens_morris | Twelve_mens_morris

val max_piece_from_template : template -> int

val init_template : template -> Type.board

val cant_move : Type.player -> Type.game_update -> bool

val lost : Type.game_update -> Type.player -> bool

val init_player : Type.color -> Type.player
