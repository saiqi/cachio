let offense_score board roster =
  Round.compute_position_score ~board ~roster ~position:Position.Forward

let defense_score board roster =
  Round.compute_position_score ~board ~roster ~position:Position.Defender

let balanced_score board roster =
  Round.compute_position_score ~board ~roster ~position:Position.Forward
  + Round.compute_position_score ~board ~roster ~position:Position.Defender
  + Round.compute_position_score ~board ~roster ~position:Position.Midfielder

let optimal_score home board roster =
  let p = Round.compute_param ~home ~board ~roster in
  Dice_count.to_int (Round_param.offensive_dices p)
  + Dice_count.to_int (Round_param.defensive_dices p)
  + Action_count.to_int (Round_param.actions p)
