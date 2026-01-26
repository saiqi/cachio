type t

val create : Ai_id.t -> Roster.t -> Strategy_id.t -> t
val id : t -> Ai_id.t
val roster : t -> Roster.t
val strategy : t -> Strategy_id.t

val build_board :
  (module Rng.S with type t = 'rng) -> 'rng -> t -> bool -> Board.t
