type t

val offense : t
val defense : t
val balanced : t
val optimal : bool -> t
val pragmatic : bool -> t
val dummy : t
val make : Strategy_id.t -> (Board.t -> Roster.t -> int) -> t

val build :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  ?generate:
    ((module Rng.S with type t = 'rng) -> 'rng -> Roster.t -> Board.t list) ->
  t ->
  Roster.t ->
  Board.t

val id : t -> Strategy_id.t
val of_id : home:bool -> Strategy_id.t -> t
