open Cachio

let fake_stats () =
  let home_param =
    Round_param.create ~offensive_dice:(Dice_count.of_int_exn 2)
      ~defensive_dice:(Dice_count.of_int_exn 1)
      ~actions:(Action_count.of_int_exn 2)
  in
  let away_param =
    Round_param.create ~offensive_dice:(Dice_count.of_int_exn 1)
      ~defensive_dice:(Dice_count.of_int_exn 2)
      ~actions:(Action_count.of_int_exn 1)
  in
  let first_result =
    Game_result.create ~home:(Ai_id.of_int 0) ~home_goals:1
      ~away:(Ai_id.of_int 1) ~away_goals:0
  in
  let second_result =
    Game_result.create ~home:(Ai_id.of_int 1) ~home_goals:0
      ~away:(Ai_id.of_int 0) ~away_goals:2
  in
  Stats.of_audits
    [
      Game_audit.create ~result:first_result ~home_param ~away_param
        ~home_strategy:Strategy_id.Offensive
        ~away_strategy:Strategy_id.Defensive;
      Game_audit.create ~result:second_result ~home_param ~away_param
        ~home_strategy:Strategy_id.Defensive
        ~away_strategy:Strategy_id.Offensive;
    ]

let test_audit_to_obs () =
  let stats = fake_stats () in
  Alcotest.check Alcotest.int "2 games -> 4 obs" 4
    (List.length (Stats.obs stats));
  let wins =
    List.filter (fun o -> Stats.outcome o = Stats.Win) (Stats.obs stats)
  in
  let losses =
    List.filter (fun o -> Stats.outcome o = Stats.Loss) (Stats.obs stats)
  in
  let draws =
    List.filter (fun o -> Stats.outcome o = Stats.Draw) (Stats.obs stats)
  in
  Alcotest.check Alcotest.int "2 wins" 2 (List.length wins);
  Alcotest.check Alcotest.int "0 draws" 0 (List.length draws);
  Alcotest.check Alcotest.int "2 losses" 2 (List.length losses)

let test_game_goals_mean () =
  let stats = fake_stats () in
  match Stats.game_goals_mean stats with
  | None -> Alcotest.fail "game goals mean is none"
  | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) "game goals mean" 1.5 v

let test_win_ratio () =
  let stats = fake_stats () in
  match Stats.win_ratio stats with
  | None -> Alcotest.fail "win ratio is none"
  | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) "win ratio" 0.5 v

let test_by_strategy () =
  let stats = Stats.by_strategy (fake_stats ()) Strategy_id.Defensive in
  Alcotest.check Alcotest.int "2 obs" 2 (List.length (Stats.obs stats))

let test_by_home () =
  let stats = Stats.by_home (fake_stats ()) in
  Alcotest.check Alcotest.int "2 obs" 2 (List.length (Stats.obs stats))

let test_by_ai () =
  let stats = Stats.by_ai (fake_stats ()) (Ai_id.of_int 0) in
  Alcotest.check Alcotest.int "2 obs" 2 (List.length (Stats.obs stats))

let test_goals_per_action () =
  let stats = fake_stats () in
  match Stats.goals_per_action stats with
  | None -> Alcotest.fail "goals per action is none"
  | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) "goals per action" 0.5 v

let suite =
  [
    ("audit to obs", `Quick, test_audit_to_obs);
    ("game goals mean", `Quick, test_game_goals_mean);
    ("win ratio", `Quick, test_win_ratio);
    ("filter by strategy", `Quick, test_by_strategy);
    ("filter by home", `Quick, test_by_home);
    ("filter by AI", `Quick, test_by_ai);
    ("goals per action", `Quick, test_goals_per_action);
  ]
