type t

val create : Player_id.t -> Position.t -> Score.t -> t
val id : t -> Player_id.t
val pos : t -> Position.t
val shape : t -> Shape.t
val score : t -> Score.t
val incr_shape : t -> t
val decr_shape : t -> t
val incr_score : t -> t
val decr_score : t -> t
val equal : t -> t -> bool
val adjust_score : t -> Position.t -> t
val is_injured : t -> bool
