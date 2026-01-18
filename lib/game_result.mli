type t

val create :
  home:Ai_id.t -> home_goals:int -> away:Ai_id.t -> away_goals:int -> t

val home : t -> Ai_id.t
val away : t -> Ai_id.t
val home_goals : t -> int
val away_goals : t -> int
val winner : t -> Ai_id.t option
