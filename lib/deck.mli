type t
type shuffle = Card.t list -> Card.t list

val empty : t
val full : t
val find : Position.t -> t -> Card.t list
val shuffle : shuffle -> t -> t
val draw : Position.t -> int -> t -> Card.t list
