type t

val create :
  result:Game_result.t ->
  home_param:Round_param.t ->
  away_param:Round_param.t ->
  home_strategy:Strategy_id.t ->
  away_strategy:Strategy_id.t ->
  home_board_hash:int ->
  away_board_hash:int ->
  t

val result : t -> Game_result.t
val home_goals : t -> int
val away_goals : t -> int
val home_id : t -> Ai_id.t
val away_id : t -> Ai_id.t
val home_actions : t -> Action_count.t
val away_actions : t -> Action_count.t
val home_offensive_dice : t -> Dice_count.t
val away_offensive_dice : t -> Dice_count.t
val home_defensive_dice : t -> Dice_count.t
val away_defensive_dice : t -> Dice_count.t
val home_strategy : t -> Strategy_id.t
val away_strategy : t -> Strategy_id.t
val home_board_hash : t -> int
val away_board_hash : t -> int
