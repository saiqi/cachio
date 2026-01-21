type t = {
  result : Game_result.t;
  home_actions : Action_count.t;
  away_actions : Action_count.t;
  home_offensive_dice : Dice_count.t;
  away_offensive_dice : Dice_count.t;
  home_defensive_dice : Dice_count.t;
  away_defensive_dice : Dice_count.t;
  home_strategy : Strategy_id.t;
  away_strategy : Strategy_id.t;
}

let create ~result ~home_param ~away_param ~home_strategy ~away_strategy =
  {
    result;
    home_actions = Round_param.actions home_param;
    away_actions = Round_param.actions away_param;
    home_offensive_dice = Round_param.offensive_dice home_param;
    away_offensive_dice = Round_param.offensive_dice away_param;
    home_defensive_dice = Round_param.defensive_dice home_param;
    away_defensive_dice = Round_param.defensive_dice away_param;
    home_strategy;
    away_strategy;
  }

let result g = g.result
let home_actions g = g.home_actions
let away_actions g = g.away_actions
let home_offensive_dice g = g.home_offensive_dice
let away_offensive_dice g = g.away_offensive_dice
let home_defensive_dice g = g.home_defensive_dice
let away_defensive_dice g = g.away_defensive_dice
let home_strategy g = g.home_strategy
let away_strategy g = g.away_strategy
