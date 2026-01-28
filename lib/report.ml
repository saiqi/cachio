type value =
  | Float of float
  | Percent of float
  | Int of int
  | Interval of (value * value)
  | Optional of value option

type metric = { name : string; value : value }
type section = { title : string; metrics : metric list }
type t = section list

let title section = section.title
let metrics section = section.metrics
let name metric = metric.name
let value metric = metric.value
let to_list report = report
let percent v = Option.map (fun x -> x *. 100.) v
let metric name value = { name; value }

let opt_float = function
  | None -> Optional None
  | Some v -> Optional (Some (Float v))

let opt_percent = function
  | None -> Optional None
  | Some v -> Optional (Some (Percent v))

let opt_interval = function
  | None -> Optional None
  | Some (l, r) -> Optional (Some (Interval (Float l, Float r)))

let global_section stats =
  {
    title = "Global";
    metrics =
      [
        metric "Total games" (Int (Stats.game_count stats));
        metric "Goal per game (mean)" (opt_float (Stats.game_goals_mean stats));
        metric "Goal per game (stddev)"
          (opt_float (Stats.game_goals_stddev stats));
        metric "Win ratio" (opt_percent (Stats.win_ratio stats |> percent));
        metric "Draw ratio" (opt_percent (Stats.draw_ratio stats |> percent));
        metric "Loss ratio" (opt_percent (Stats.loss_ratio stats |> percent));
        metric "Goals per action" (opt_float (Stats.goals_per_action stats));
        metric "Actions (mean)" (opt_float (Stats.actions_mean stats));
        metric "Actions (stddev)" (opt_float (Stats.actions_stddev stats));
        metric "Offensive dice (mean)"
          (opt_float (Stats.offensive_dice_mean stats));
        metric "Offensive dice (stddev)"
          (opt_float (Stats.offensive_dice_stddev stats));
        metric "Defensive dice (mean)"
          (opt_float (Stats.defensive_dice_mean stats));
        metric "Defensive dice (stddev)"
          (opt_float (Stats.defensive_dice_stddev stats));
        metric "Offensive dice per goals"
          (opt_float (Stats.offensive_dice_per_goal stats));
        metric "Defensive dice per goals conceded"
          (opt_float (Stats.defensive_dice_per_goal_conceded stats));
      ];
  }

let home_advantage_section stats =
  let home_stats = Stats.by_home stats in
  {
    title = "Home advantage";
    metrics =
      [
        metric "Win ratio" (opt_percent (Stats.win_ratio home_stats |> percent));
        metric "Draw ratio"
          (opt_percent (Stats.draw_ratio home_stats |> percent));
        metric "Loss ratio"
          (opt_percent (Stats.loss_ratio home_stats |> percent));
      ];
  }

let strategy_section stats =
  List.map
    (fun s ->
      let strat_stats = Stats.by_strategy stats s in
      {
        title = Strategy_id.to_string s;
        metrics =
          [
            metric "Win rate CI" (opt_interval (Stats.win_rate_ci strat_stats));
            metric "Goals per action"
              (opt_float (Stats.goals_per_action strat_stats));
            metric "Actions (mean)" (opt_float (Stats.actions_mean strat_stats));
            metric "Actions (stddev)"
              (opt_float (Stats.actions_stddev strat_stats));
            metric "Offensive dice (mean)"
              (opt_float (Stats.offensive_dice_mean strat_stats));
            metric "Offensive dice (stddev)"
              (opt_float (Stats.offensive_dice_stddev strat_stats));
            metric "Defensive dice (mean)"
              (opt_float (Stats.defensive_dice_mean strat_stats));
            metric "Defensive dice (stddev)"
              (opt_float (Stats.defensive_dice_stddev strat_stats));
            metric "Offensive dice per goals"
              (opt_float (Stats.offensive_dice_per_goal strat_stats));
            metric "Defensive dice per goals conceded"
              (opt_float (Stats.defensive_dice_per_goal_conceded strat_stats));
            metric "Board normalized entropy"
              (opt_float (Stats.board_normalized_entropy strat_stats));
          ];
      })
    Strategy_id.all

let of_stats stats =
  [ global_section stats; home_advantage_section stats ]
  @ strategy_section stats
