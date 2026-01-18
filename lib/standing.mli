type t
type entry

val empty : t
val init : Ai_id.t list -> t
val update : t -> Game_result.t -> t
val played : Ai_id.t -> t -> int
val wins : Ai_id.t -> t -> int
val draws : Ai_id.t -> t -> int
val losses : Ai_id.t -> t -> int
val points : Ai_id.t -> t -> int
val goals_for : Ai_id.t -> t -> int
val goals_against : Ai_id.t -> t -> int
