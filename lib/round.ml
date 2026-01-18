let compute_position_score ~board ~roster ~position =
  let players_in_position = Board.player_on_rows board (Board.row position) in
  List.fold_left
    (fun acc player_id ->
      let player = Roster.find player_id roster in
      let s = Rules.adjust_score player position |> Score.to_int in
      acc + s)
    0 players_in_position

let compute_param ~home ~board ~roster =
  let offensive_score =
    compute_position_score ~board ~roster ~position:Position.Forward
  in
  let midfield_score =
    compute_position_score ~board ~roster ~position:Position.Midfielder
  in
  let defensive_score =
    compute_position_score ~board ~roster ~position:Position.Defender
  in
  let offensive_dices =
    Piecewise.eval Balance.offensive_dice_curve offensive_score
    |> Dice_count.of_int_exn
  in
  let defensive_dices =
    Piecewise.eval Balance.defensive_dice_curve defensive_score
    |> Dice_count.of_int_exn
  in
  let actions =
    Piecewise.eval Balance.action_curve midfield_score
    + Rules.adjust_home_advantage home
    |> Action_count.of_int_exn
  in
  Round_param.create ~offensive_dices ~defensive_dices ~actions

let resolve_action (type a) (module R : Rng.S with type t = a) (rng : a)
    ~attacker ~defender =
  let total_attacker =
    Dice.roll (module R) rng (Round_param.offensive_dices attacker)
  in
  let total_defender =
    Dice.roll (module R) rng (Round_param.defensive_dices defender)
  in
  Rules.has_scored total_attacker total_defender

let resolve (type a) (module R : Rng.S with type t = a) (rng : a) ~home ~away =
  let h_actions = Action_count.to_int (Round_param.actions home) in
  let a_actions = Action_count.to_int (Round_param.actions away) in
  let steps = max h_actions a_actions in
  let step (h_goals, a_goals) i =
    let h_goals =
      if
        i < h_actions
        && resolve_action (module R) rng ~attacker:home ~defender:away
      then h_goals + 1
      else h_goals
    in
    let a_goals =
      if
        i < a_actions
        && resolve_action (module R) rng ~attacker:away ~defender:home
      then a_goals + 1
      else a_goals
    in
    (h_goals, a_goals)
  in
  List.init steps Fun.id |> List.fold_left step (0, 0)
