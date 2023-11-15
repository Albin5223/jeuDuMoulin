val equals_board : Mill.Type.board -> Mill.Type.board -> bool

val equals_coordinate : Mill.Type.coordinates -> Mill.Type.coordinates -> bool

val equals_list_coordinate : Mill.Type.coordinates list -> Mill.Type.coordinates list -> bool

val equals_player : Mill.Type.player -> Mill.Type.player -> bool

val equals_end_game : Mill.Type.end_game -> Mill.Type.end_game -> bool

val game_update_of_game : Mill.Type.end_game -> Mill.Type.game_update

val square_reachable_from_coordinates : int * int -> Mill.Type.board -> Mill.Type.board

val board_map_all : (Mill.Type.square -> Mill.Type.square) -> Mill.Type.board -> Mill.Type.board

val for_all_board : (Mill.Type.square -> bool) -> Mill.Type.board -> bool

val fill_all_node : Mill.Type.template -> Mill.Type.color -> Mill.Type.board

val test_complete_board : Mill.Type.board -> bool

val generate_color : Mill.Type.color QCheck.Gen.t

val arbitrary_color : Mill.Type.color QCheck.arbitrary

val generate_templates : Mill.Type.template QCheck.Gen.t

val arbitrary_templates : Mill.Type.template QCheck.arbitrary

val player_random_dumb : (int -> int) -> Mill.Type.player_strategie
