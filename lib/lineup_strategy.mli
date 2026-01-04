type t

val offense : t
val defense : t
val balanced : t
val optimal : bool -> t
val make : (Board.t -> Roster.t -> int) -> t
val build : ?generate:(Roster.t -> Board.t list) -> t -> Roster.t -> Board.t
