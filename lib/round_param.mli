type t

val create :
  offensive_dices:Dice_count.t ->
  defensive_dices:Dice_count.t ->
  actions:Action_count.t ->
  t

val offensive_dices : t -> Dice_count.t
val defensive_dices : t -> Dice_count.t
val actions : t -> Action_count.t
