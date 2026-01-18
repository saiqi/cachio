type t

val empty : t
val full : t
val find : Position.t -> t -> Card.t list
val shuffle : (module Rng.S with type t = 'rng) -> 'rng -> t -> t
val draw : Position.t -> int -> t -> t * Card.t list
