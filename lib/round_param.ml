type t = {
  offensive_dices : Dice_count.t;
  defensive_dices : Dice_count.t;
  actions : Action_count.t;
}

let create ~offensive_dices ~defensive_dices ~actions =
  { offensive_dices; defensive_dices; actions }

let offensive_dices x = x.offensive_dices
let defensive_dices x = x.defensive_dices
let actions x = x.actions
