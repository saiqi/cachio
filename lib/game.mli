val play :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  home:Ai.t ->
  away:Ai.t ->
  Game_result.t

val play_with_audit :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  home:Ai.t ->
  away:Ai.t ->
  Game_audit.t
