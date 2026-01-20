val run :
  (module Rng.S with type t = 'rng) ->
  'rng ->
  Participants.t ->
  Schedule.t ->
  Standing.t
