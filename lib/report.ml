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

let rec value_to_yojson = function
  | Float f -> `Assoc [ ("type", `String "float"); ("value", `Float f) ]
  | Percent p -> `Assoc [ ("type", `String "percent"); ("value", `Float p) ]
  | Int i -> `Assoc [ ("type", `String "int"); ("value", `Int i) ]
  | Interval (l, r) ->
      `Assoc
        [
          ("type", `String "interval");
          ("lower", value_to_yojson l);
          ("upper", value_to_yojson r);
        ]
  | Optional None -> `Assoc [ ("type", `String "optional"); ("value", `Null) ]
  | Optional (Some v) ->
      `Assoc [ ("type", `String "optional"); ("value", value_to_yojson v) ]

let metric_to_yojson m =
  `Assoc [ ("name", `String m.name); ("value", value_to_yojson m.value) ]

let section_to_yojson s =
  `Assoc
    [
      ("title", `String s.title);
      ("metrics", `List (List.map metric_to_yojson s.metrics));
    ]

let to_yojson report = `List (List.map section_to_yojson report)

let assoc key fields =
  match List.assoc_opt key fields with
  | Some v -> Ok v
  | None -> Error ("missing field: " ^ key)

let string_field key fields =
  match assoc key fields with
  | Error e -> Error e
  | Ok (`String s) -> Ok s
  | Ok _ -> Error ("invalid string field: " ^ key)

let rec value_of_yojson = function
  | `Assoc fields -> (
      match string_field "type" fields with
      | Error e -> Error e
      | Ok "float" -> (
          match assoc "value" fields with
          | Ok (`Float f) -> Ok (Float f)
          | Ok (`Int i) -> Ok (Float (float_of_int i))
          | Ok _ -> Error "invalid float value"
          | Error e -> Error e)
      | Ok "percent" -> (
          match assoc "value" fields with
          | Ok (`Float f) -> Ok (Percent f)
          | Ok (`Int i) -> Ok (Percent (float_of_int i))
          | Ok _ -> Error "invalid percent value"
          | Error e -> Error e)
      | Ok "int" -> (
          match assoc "value" fields with
          | Ok (`Int i) -> Ok (Int i)
          | Ok _ -> Error "invalid int value"
          | Error e -> Error e)
      | Ok "interval" -> (
          match (assoc "lower" fields, assoc "upper" fields) with
          | Ok lower, Ok upper -> (
              match (value_of_yojson lower, value_of_yojson upper) with
              | Ok lower, Ok upper -> Ok (Interval (lower, upper))
              | Error e, _ | _, Error e -> Error e)
          | Error e, _ | _, Error e -> Error e)
      | Ok "optional" -> (
          match assoc "value" fields with
          | Ok `Null -> Ok (Optional None)
          | Ok value -> (
              match value_of_yojson value with
              | Ok v -> Ok (Optional (Some v))
              | Error e -> Error e)
          | Error e -> Error e)
      | Ok kind -> Error ("unknown value type: " ^ kind))
  | _ -> Error "invalid value"

let metric_of_yojson = function
  | `Assoc fields -> (
      match (string_field "name" fields, assoc "value" fields) with
      | Ok name, Ok value -> (
          match value_of_yojson value with
          | Ok value -> Ok { name; value }
          | Error e -> Error e)
      | Error e, _ | _, Error e -> Error e)
  | _ -> Error "invalid metric"

let section_of_yojson = function
  | `Assoc fields -> (
      match (string_field "title" fields, assoc "metrics" fields) with
      | Ok title, Ok (`List metrics) ->
          let rec parse acc = function
            | [] -> Ok { title; metrics = List.rev acc }
            | metric :: rest -> (
                match metric_of_yojson metric with
                | Ok metric -> parse (metric :: acc) rest
                | Error e -> Error e)
          in
          parse [] metrics
      | Ok _, Ok _ -> Error "invalid metrics field"
      | Error e, _ | _, Error e -> Error e)
  | _ -> Error "invalid section"

let of_yojson = function
  | `List sections ->
      let rec parse acc = function
        | [] -> Ok (List.rev acc)
        | section :: rest -> (
            match section_of_yojson section with
            | Ok section -> parse (section :: acc) rest
            | Error e -> Error e)
      in
      parse [] sections
  | _ -> Error "invalid report"

let global_section stats =
  {
    title = "Global";
    metrics =
      [
        metric "Total games" (Int (Stats.game_count stats));
        metric "Point spread (mean)" (opt_float (Stats.point_spread_mean stats));
        metric "Distinct point totals (mean)"
          (opt_float (Stats.distinct_point_totals_mean stats));
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
        metric "Win / initial draw dependency"
          (opt_float (Stats.win_initial_draw_dependency stats));
        metric "Worst initial draw win ratio"
          (opt_percent (Stats.worst_initial_draw_win_ratio stats |> percent));
        metric "Best initial draw win ratio"
          (opt_percent (Stats.best_initial_draw_win_ratio stats |> percent));
        metric "Initial draw win-rate spread"
          (opt_percent (Stats.initial_draw_win_rate_spread stats |> percent));
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
            metric "Tactic normalized entropy"
              (opt_float (Stats.tactic_normalized_entropy strat_stats));
          ];
      })
    Strategy_id.all

let of_stats stats =
  [ global_section stats; home_advantage_section stats ]
  @ strategy_section stats
