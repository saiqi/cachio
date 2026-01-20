val play :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  home:Ai.t ->
  away:Ai.t ->
  Game_result.t
