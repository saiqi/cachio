type outcome = Win | Draw | Loss

type obs = {
  ai : Ai_id.t;
  strategy : Strategy_id.t;
  home : bool;
  goals_for : int;
  goals_against : int;
  actions : Action_count.t;
  offensive_dice : Dice_count.t;
  defensive_dice : Dice_count.t;
  outcome : outcome;
}

type t = { games : Game_audit.t list; obs : obs list }

let obs_of_audit audit =
  let home_goals = Game_audit.home_goals audit in
  let away_goals = Game_audit.away_goals audit in
  [
    {
      ai = Game_audit.home_id audit;
      strategy = Game_audit.home_strategy audit;
      home = true;
      goals_for = home_goals;
      goals_against = away_goals;
      actions = Game_audit.home_actions audit;
      offensive_dice = Game_audit.home_offensive_dice audit;
      defensive_dice = Game_audit.home_defensive_dice audit;
      outcome =
        (if home_goals > away_goals then Win
         else if home_goals = away_goals then Draw
         else Loss);
    };
    {
      ai = Game_audit.away_id audit;
      strategy = Game_audit.away_strategy audit;
      home = false;
      goals_for = away_goals;
      goals_against = home_goals;
      actions = Game_audit.away_actions audit;
      offensive_dice = Game_audit.away_offensive_dice audit;
      defensive_dice = Game_audit.away_defensive_dice audit;
      outcome =
        (if home_goals < away_goals then Win
         else if home_goals = away_goals then Draw
         else Loss);
    };
  ]

let obs stats = stats.obs
let games stats = stats.games
let outcome obs = obs.outcome
let to_obs games = games |> List.map obs_of_audit |> List.flatten
let of_audits games = { games; obs = to_obs games }

let mean l =
  if List.length l = 0 then None
  else
    let total = List.fold_left (fun acc e -> acc +. float_of_int e) 0. l in
    Some (total /. float_of_int (List.length l))

let var l =
  if List.length l < 2 then None
  else
    let m = mean l in
    match m with
    | None -> None
    | Some v ->
        let sum_sq_diff =
          List.fold_left
            (fun acc e -> acc +. ((float_of_int e -. v) ** 2.0))
            0.0 l
        in
        Some (sum_sq_diff /. float_of_int (List.length l - 1))

let total_game_goals games =
  List.map (fun g -> Game_audit.home_goals g + Game_audit.away_goals g) games

let game_goals_mean stats = stats.games |> total_game_goals |> mean

let game_goals_stddev stats =
  stats.games |> total_game_goals |> var |> Option.map sqrt

let game_count stats = List.length stats.games

let ratio l =
  if List.length l = 0 then None
  else
    let num, denom =
      List.fold_left
        (fun acc o ->
          let l1, r1 = acc in
          let l2, r2 = o in
          (l1 +. float_of_int l2, r1 +. float_of_int r2))
        (0., 0.) l
    in
    Some (num /. denom)

let outcome_ratio outcome obs =
  ratio (List.map (fun o -> ((if o.outcome = outcome then 1 else 0), 1)) obs)

let win_ratio stats = outcome_ratio Win stats.obs
let draw_ratio stats = outcome_ratio Draw stats.obs
let loss_ratio stats = outcome_ratio Loss stats.obs
let filter_obs f stats = { stats with obs = List.filter f stats.obs }
let by_home stats = filter_obs (fun o -> o.home) stats

let by_strategy stats strategy =
  filter_obs (fun o -> o.strategy = strategy) stats

let by_ai stats ai = filter_obs (fun o -> o.ai = ai) stats
let obs_ratio stats f = stats.obs |> List.map f |> ratio

let goals_per_action stats =
  obs_ratio stats (fun o -> (o.goals_for, o.actions |> Action_count.to_int))

let offensive_dice_per_goal stats =
  obs_ratio stats (fun o ->
      (o.offensive_dice |> Dice_count.to_int, o.goals_for))

let defensive_dice_per_goal_conceded stats =
  obs_ratio stats (fun o ->
      (o.defensive_dice |> Dice_count.to_int, o.goals_against))

let obs_mean stats f = stats.obs |> List.map f |> mean
let obs_stddev stats f = stats.obs |> List.map f |> var |> Option.map sqrt

let actions_mean stats =
  obs_mean stats (fun o -> o.actions |> Action_count.to_int)

let actions_stddev stats =
  obs_stddev stats (fun o -> o.actions |> Action_count.to_int)

let offensive_dice_mean stats =
  obs_mean stats (fun o -> o.offensive_dice |> Dice_count.to_int)

let offensive_dice_stddev stats =
  obs_stddev stats (fun o -> o.offensive_dice |> Dice_count.to_int)

let defensive_dice_mean stats =
  obs_mean stats (fun o -> o.defensive_dice |> Dice_count.to_int)

let defensive_dice_stddev stats =
  obs_stddev stats (fun o -> o.defensive_dice |> Dice_count.to_int)
