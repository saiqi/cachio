type cell = Empty | Occupied of Player_id.t
type t

val empty : t
val get : t -> Row.t -> Column.t -> cell
val place : Player_id.t -> Row.t -> Column.t -> t -> t
val remove : Row.t -> Column.t -> t -> t
val player_on_rows : t -> Row.t -> Player_id.t list
val position : Row.t -> Position.t
val row : Position.t -> Row.t
val of_list : (Player_id.t * Row.t * Column.t) list -> t
val to_list : t -> (Player_id.t * Row.t * Column.t) list
val is_valid : t -> bool
