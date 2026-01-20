type game
type t

val round_robin : Ai_id.t list -> t
val day : t -> int -> game list
val home : game -> Ai_id.t
val away : game -> Ai_id.t
val length : t -> int
val to_list : t -> game list list
val of_list : (Ai_id.t * Ai_id.t) list list -> t
