type t

val create :
  offensive_dice:Dice_count.t ->
  defensive_dice:Dice_count.t ->
  actions:Action_count.t ->
  t

val offensive_dice : t -> Dice_count.t
val defensive_dice : t -> Dice_count.t
val actions : t -> Action_count.t
val hash : t -> int
