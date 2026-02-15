type t

val of_ai : Ai.t -> t
val of_scripted : Scripted.t -> t
val id : t -> Ai_id.t
val strategy : t -> Strategy_id.t
val roster : t -> Roster.t option
val after_game : Game_result.t -> t -> t

val reveal :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  home:bool ->
  t ->
  Round_param.t * Board.t option
