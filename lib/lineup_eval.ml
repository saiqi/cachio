let offense_score board roster =
  (2 * Round.compute_position_score ~board ~roster ~position:Position.Forward)
  + Round.compute_position_score ~board ~roster ~position:Position.Midfielder

let defense_score board roster =
  (2 * Round.compute_position_score ~board ~roster ~position:Position.Defender)
  + Round.compute_position_score ~board ~roster ~position:Position.Midfielder

let balanced_score board roster =
  Round.compute_position_score ~board ~roster ~position:Position.Forward
  + Round.compute_position_score ~board ~roster ~position:Position.Defender
  + Round.compute_position_score ~board ~roster ~position:Position.Midfielder

let pragmatic_score home board roster =
  if home then offense_score board roster else defense_score board roster

let optimal_score home board roster =
  let p = Round.compute_param ~home ~board ~roster in
  Dice_count.to_int (Round_param.offensive_dice p)
  + Dice_count.to_int (Round_param.defensive_dice p)
  + Action_count.to_int (Round_param.actions p)

let dummy_score _ _ = 0
