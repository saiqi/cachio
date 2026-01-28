val run :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  Participants.t ->
  Schedule.t ->
  Standing.t

val run_with_audit :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  Participants.t ->
  Schedule.t ->
  Participants.t * Standing.t * Game_audit.t list
