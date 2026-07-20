type outcome = Win | Draw | Loss
type obs
type t

val of_audits : Game_audit.t list -> t
val of_runs : (Standing.t * Game_audit.t list) list -> t
val obs : t -> obs list
val outcome : obs -> outcome
val games : t -> Game_audit.t list
val game_goals_mean : t -> float option
val game_goals_stddev : t -> float option
val win_ratio : t -> float option
val draw_ratio : t -> float option
val loss_ratio : t -> float option
val by_home : t -> t
val by_strategy : t -> Strategy_id.t -> t
val by_ai : t -> Ai_id.t -> t
val goals_per_action : t -> float option
val offensive_dice_per_goal : t -> float option
val defensive_dice_per_goal_conceded : t -> float option
val actions_mean : t -> float option
val actions_stddev : t -> float option
val offensive_dice_mean : t -> float option
val offensive_dice_stddev : t -> float option
val defensive_dice_mean : t -> float option
val defensive_dice_stddev : t -> float option
val game_count : t -> int
val point_spread_mean : t -> float option
val distinct_point_totals_mean : t -> float option
val board_entropy : t -> float option
val board_normalized_entropy : t -> float option
val tactic_entropy : t -> float option
val tactic_normalized_entropy : t -> float option
val win_initial_draw_dependency : t -> float option
val worst_initial_draw_win_ratio : t -> float option
val best_initial_draw_win_ratio : t -> float option
val initial_draw_win_rate_spread : t -> float option
val win_rate_ci : t -> (float * float) option
