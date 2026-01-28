type t = {
  offensive_dice : Dice_count.t;
  defensive_dice : Dice_count.t;
  actions : Action_count.t;
}

let create ~offensive_dice ~defensive_dice ~actions =
  { offensive_dice; defensive_dice; actions }

let offensive_dice x = x.offensive_dice
let defensive_dice x = x.defensive_dice
let actions x = x.actions

let hash x =
  (100 * Dice_count.to_int x.offensive_dice)
  + (10 * Dice_count.to_int x.defensive_dice)
  + Action_count.to_int x.actions
