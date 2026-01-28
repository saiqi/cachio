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
  home_board_shape : int;
  away_board_shape : int;
  home_tactic : int;
  away_tactic : int;
}

let create ~result ~home_param ~away_param ~home_strategy ~away_strategy
    ~home_board ~away_board =
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
    home_board_shape = Board_symmetry.canonical_hash home_board;
    away_board_shape = Board_symmetry.canonical_hash away_board;
    home_tactic = Round_param.hash home_param;
    away_tactic = Round_param.hash away_param;
  }

let result g = g.result
let home_goals g = Game_result.home_goals g.result
let away_goals g = Game_result.away_goals g.result
let home_id g = Game_result.home g.result
let away_id g = Game_result.away g.result
let home_actions g = g.home_actions
let away_actions g = g.away_actions
let home_offensive_dice g = g.home_offensive_dice
let away_offensive_dice g = g.away_offensive_dice
let home_defensive_dice g = g.home_defensive_dice
let away_defensive_dice g = g.away_defensive_dice
let home_strategy g = g.home_strategy
let away_strategy g = g.away_strategy
let home_board_shape g = g.home_board_shape
let away_board_shape g = g.away_board_shape
let home_tactic g = g.home_tactic
let away_tactic g = g.away_tactic
