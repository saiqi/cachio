type t = {
  result : Game_result.t;
  home_actions : int;
  away_actions : int;
  home_offensive_dices : int;
  away_offensive_dices : int;
  home_defensive_dices : int;
  away_defensive_dices : int;
}

let create ~result ~home_param ~away_param =
  {
    result;
    home_actions = Round_param.actions home_param |> Action_count.to_int;
    away_actions = Round_param.actions away_param |> Action_count.to_int;
    home_offensive_dices =
      Round_param.offensive_dices home_param |> Dice_count.to_int;
    away_offensive_dices =
      Round_param.offensive_dices away_param |> Dice_count.to_int;
    home_defensive_dices =
      Round_param.defensive_dices home_param |> Dice_count.to_int;
    away_defensive_dices =
      Round_param.defensive_dices away_param |> Dice_count.to_int;
  }

let result g = g.result
let home_actions g = g.home_actions
let away_actions g = g.away_actions
let home_offensive_dices g = g.home_offensive_dices
let away_offensive_dices g = g.away_offensive_dices
let home_defensive_dices g = g.home_defensive_dices
let away_defensive_dices g = g.away_defensive_dices
