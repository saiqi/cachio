type t

val create : Ai_id.t -> Roster.t -> Lineup_strategy.t -> t
val id : t -> Ai_id.t
val roster : t -> Roster.t
val strategy : t -> Lineup_strategy.t
val build_board : t -> Board.t
