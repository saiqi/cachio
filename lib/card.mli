type t

val create : Position.t -> Score.t -> t
val position : t -> Position.t
val score : t -> Score.t
