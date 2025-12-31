type cell = Empty | Occupied of Player_id.t
type t

val empty : t
val get : t -> Row.t -> Column.t -> cell
val place : Player_id.t -> Row.t -> Column.t -> t -> t
val remove : Row.t -> Column.t -> t -> t
val player_on_rows : t -> Row.t -> Player_id.t list
