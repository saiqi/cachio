type t

val of_ais : Ai.t list -> t

val run :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  t ->
  Standing.t * Game_audit.t list

val run_n :
  int ->
  (module Rng.S with type t = 'rng) ->
  'rng ->
  t ->
  (Standing.t * Game_audit.t list) list
