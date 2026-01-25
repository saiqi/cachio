val offense_score : Board.t -> Roster.t -> int
val defense_score : Board.t -> Roster.t -> int
val balanced_score : Board.t -> Roster.t -> int
val optimal_score : bool -> Board.t -> Roster.t -> int
val pragmatic_score : bool -> Board.t -> Roster.t -> int
val dummy_score : Board.t -> Roster.t -> int
