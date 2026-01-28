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
  board_shape : int;
  tactic : int;
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
      board_shape = Game_audit.home_board_shape audit;
      tactic = Game_audit.home_tactic audit;
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
      board_shape = Game_audit.away_board_shape audit;
      tactic = Game_audit.away_tactic audit;
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

module IntMap = Map.Make (Int)

let frequencies f stats =
  List.fold_left
    (fun acc o ->
      IntMap.update (f o)
        (function None -> Some 1 | Some v -> Some (v + 1))
        acc)
    IntMap.empty stats.obs

let entropy f stats =
  let n = List.length stats.obs in
  if n = 0 then None
  else
    let freqs = frequencies f stats in
    let total = float_of_int n in
    let h =
      IntMap.fold
        (fun _ count acc ->
          let p = float_of_int count /. total in
          acc -. (p *. log p))
        freqs 0.0
    in
    Some h

let normalized_entropy f stats =
  let freqs = frequencies f stats in
  let k = IntMap.cardinal freqs in
  if k <= 1 then Some 0.
  else
    match entropy f stats with
    | None -> None
    | Some h -> Some (h /. log (float_of_int k))

let board_entropy stats = entropy (fun o -> o.board_shape) stats

let board_normalized_entropy stats =
  normalized_entropy (fun o -> o.board_shape) stats

let tactic_entropy stats = entropy (fun o -> o.tactic) stats

let tactic_normalized_entropy stats =
  normalized_entropy (fun o -> o.tactic) stats

let wilson_interval ~ones ~obs =
  let total = List.length obs in
  if total = 0 then None
  else
    let z = 1.96 in
    let n = float_of_int total in
    let w = float_of_int (List.length ones) in
    let phat = w /. n in

    let z2 = z *. z in
    let denom = 1. +. (z2 /. n) in

    let center = (phat +. (z2 /. (2. *. n))) /. denom in
    let half_width =
      z *. sqrt (((phat *. (1. -. phat)) +. (z2 /. (4. *. n))) /. n) /. denom
    in
    Some (center -. half_width, center +. half_width)

let win_rate_ci stats =
  wilson_interval
    ~ones:(List.filter (fun o -> o.outcome = Win) stats.obs)
    ~obs:stats.obs
