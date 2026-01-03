type t

val empty : t
val add : Player.t -> t -> t
val find_opt : Player_id.t -> t -> Player.t option
val find : Player_id.t -> t -> Player.t
val of_list : Player.t list -> t
