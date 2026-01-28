val run :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  (Ai_id.t * Strategy_id.t) list ->
  Standing.t * Game_audit.t list

val run_n :
  int ->
  (module Rng.S with type t = 'rng) ->
  'rng ->
  (Ai_id.t * Strategy_id.t) list ->
  (Standing.t * Game_audit.t list) list
