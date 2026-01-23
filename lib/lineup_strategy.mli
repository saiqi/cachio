type t

val offense : t
val defense : t
val balanced : t
val optimal : bool -> t
val make : Strategy_id.t -> (Board.t -> Roster.t -> int) -> t
val build : ?generate:(Roster.t -> Board.t list) -> t -> Roster.t -> Board.t
val id : t -> Strategy_id.t
val of_id : home:bool -> Strategy_id.t -> t
