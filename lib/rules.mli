val is_injured : Player.t -> bool
val adjust_score : Player.t -> Position.t -> Score.t
val has_scored : int -> int -> bool
val adjust_home_advantage : bool -> int
val min_players_on_row : int
val players_on_board : int
val players_on_roster : int
