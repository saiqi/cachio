val draw : int -> 'a list -> 'a list * 'a list
val take : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val shuffle : (module Rng.S with type t = 'rng) -> 'rng -> 'a list -> 'a list
