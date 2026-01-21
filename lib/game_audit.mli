type t

val create :
  result:Game_result.t ->
  home_param:Round_param.t ->
  away_param:Round_param.t ->
  t

val result : t -> Game_result.t
val home_actions : t -> int
val away_actions : t -> int
val home_offensive_dices : t -> int
val away_offensive_dices : t -> int
val home_defensive_dices : t -> int
val away_defensive_dices : t -> int
