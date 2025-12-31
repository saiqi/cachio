type position = DEFENDER | MIDFIELD | FORWARD
type t

val string_of_position : position -> string
val create : Player_id.t -> position -> Score.t -> t
val id : t -> Player_id.t
val pos : t -> position
val shape : t -> Shape.t
val score : t -> Score.t
val incr_shape : t -> t
val decr_shape : t -> t
val incr_score : t -> t
val decr_score : t -> t
val adjust_score : t -> position -> Score.t
val is_injured : t -> bool
