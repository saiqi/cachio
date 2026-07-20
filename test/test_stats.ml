open Cachio

let fake_stats =
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
  let home_board =
    Board.of_list
      [
        (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 0);
        (Player_id.of_int 1, Row.of_int_exn 0, Column.of_int_exn 1);
        (Player_id.of_int 2, Row.of_int_exn 1, Column.of_int_exn 0);
        (Player_id.of_int 3, Row.of_int_exn 1, Column.of_int_exn 1);
        (Player_id.of_int 4, Row.of_int_exn 2, Column.of_int_exn 0);
        (Player_id.of_int 5, Row.of_int_exn 2, Column.of_int_exn 1);
      ]
  in
  let away_board =
    Board.of_list
      [
        (Player_id.of_int 6, Row.of_int_exn 0, Column.of_int_exn 3);
        (Player_id.of_int 7, Row.of_int_exn 0, Column.of_int_exn 2);
        (Player_id.of_int 8, Row.of_int_exn 1, Column.of_int_exn 3);
        (Player_id.of_int 9, Row.of_int_exn 1, Column.of_int_exn 2);
        (Player_id.of_int 10, Row.of_int_exn 2, Column.of_int_exn 3);
        (Player_id.of_int 11, Row.of_int_exn 2, Column.of_int_exn 2);
      ]
  in
  let first_result =
    Game_result.create ~home:(Ai_id.of_int 0) ~home_goals:1
      ~away:(Ai_id.of_int 1) ~away_goals:0
      ~home_players:(Board.players home_board)
      ~away_players:(Board.players away_board)
  in
  let second_result =
    Game_result.create ~home:(Ai_id.of_int 1) ~home_goals:0
      ~away:(Ai_id.of_int 0) ~away_goals:2
      ~away_players:(Board.players home_board)
      ~home_players:(Board.players away_board)
  in
  Stats.of_audits
    [
      Game_audit.create ~result:first_result ~home_param ~away_param
        ~home_strategy:Strategy_id.Offensive
        ~away_strategy:Strategy_id.Defensive ~home_initial_draw_score:(Some 30)
        ~away_initial_draw_score:(Some 20) ~home_board:(Some home_board)
        ~away_board:(Some away_board);
      Game_audit.create ~result:second_result ~home_param ~away_param
        ~home_strategy:Strategy_id.Defensive
        ~away_strategy:Strategy_id.Offensive ~home_initial_draw_score:(Some 20)
        ~away_initial_draw_score:(Some 30) ~home_board:(Some home_board)
        ~away_board:(Some away_board);
    ]

let test_audit_to_obs () =
  let stats = fake_stats in
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
  let stats = fake_stats in
  match Stats.game_goals_mean stats with
  | None -> Alcotest.fail "game goals mean is none"
  | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) "game goals mean" 1.5 v

let test_win_ratio () =
  let stats = fake_stats in
  match Stats.win_ratio stats with
  | None -> Alcotest.fail "win ratio is none"
  | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) "win ratio" 0.5 v

let test_by_strategy () =
  let stats = Stats.by_strategy fake_stats Strategy_id.Defensive in
  Alcotest.check Alcotest.int "2 obs" 2 (List.length (Stats.obs stats))

let test_by_home () =
  let stats = Stats.by_home fake_stats in
  Alcotest.check Alcotest.int "2 obs" 2 (List.length (Stats.obs stats))

let test_by_ai () =
  let stats = Stats.by_ai fake_stats (Ai_id.of_int 0) in
  Alcotest.check Alcotest.int "2 obs" 2 (List.length (Stats.obs stats))

let test_goals_per_action () =
  let stats = fake_stats in
  match Stats.goals_per_action stats with
  | None -> Alcotest.fail "goals per action is none"
  | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) "goals per action" 0.5 v

let test_board_entropy () =
  let stats = fake_stats in
  let off_stats = Stats.by_strategy stats Strategy_id.Offensive in
  match Stats.board_entropy off_stats with
  | None -> Alcotest.fail "entropy is none"
  | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) "board entropy" 0. v

let test_win_rate_ci () =
  let stats = fake_stats in
  let ai_stats = Stats.by_ai stats (Ai_id.of_int 0) in
  match Stats.win_rate_ci ai_stats with
  | None -> Alcotest.fail "ci is none"
  | Some (l, h) ->
      Alcotest.check Alcotest.bool "between 0 and 1" true
        (l >= 0. && l <= 1. && h >= 0. && l <= 1.)

let test_win_initial_draw_dependency () =
  let stats = fake_stats in
  match Stats.win_initial_draw_dependency stats with
  | None -> Alcotest.fail "dependency is none"
  | Some v ->
      Alcotest.(check (Alcotest.float 1.e-6))
        "perfect initial draw dependency" 1. v

let test_initial_draw_tail_win_ratios () =
  let stats = fake_stats in
  let check_optional_float label expected value =
    match value with
    | None -> Alcotest.fail (label ^ " is none")
    | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) label expected v
  in
  check_optional_float "worst initial draw win ratio" 0.
    (Stats.worst_initial_draw_win_ratio stats);
  check_optional_float "best initial draw win ratio" 1.
    (Stats.best_initial_draw_win_ratio stats);
  check_optional_float "initial draw win-rate spread" 1.
    (Stats.initial_draw_win_rate_spread stats)

let test_championship_point_metrics_without_standings () =
  Alcotest.check Alcotest.bool "point spread is none" true
    (Stats.point_spread_mean fake_stats = None);
  Alcotest.check Alcotest.bool "distinct point totals is none" true
    (Stats.distinct_point_totals_mean fake_stats = None)

let test_championship_point_metrics () =
  let ids = List.init 4 Ai_id.of_int in
  let team0 = List.nth ids 0 in
  let team1 = List.nth ids 1 in
  let team2 = List.nth ids 2 in
  let team3 = List.nth ids 3 in
  let result ~home ~home_goals ~away ~away_goals =
    Game_result.create ~home ~home_goals ~away ~away_goals ~home_players:[]
      ~away_players:[]
  in
  let apply_result result standing = Standing.update standing result in
  let standing =
    Standing.init ids
    |> apply_result (result ~home:team0 ~home_goals:1 ~away:team1 ~away_goals:0)
    |> apply_result (result ~home:team2 ~home_goals:1 ~away:team3 ~away_goals:1)
    |> apply_result (result ~home:team0 ~home_goals:1 ~away:team2 ~away_goals:0)
  in
  let tied_standing =
    Standing.init ids
    |> apply_result (result ~home:team0 ~home_goals:1 ~away:team1 ~away_goals:1)
    |> apply_result (result ~home:team2 ~home_goals:1 ~away:team3 ~away_goals:1)
  in
  let stats = Stats.of_runs [ (standing, []); (tied_standing, []) ] in
  let check_optional_float label expected value =
    match value with
    | None -> Alcotest.fail (label ^ " is none")
    | Some v -> Alcotest.(check (Alcotest.float 1.e-6)) label expected v
  in
  check_optional_float "point spread mean" 3. (Stats.point_spread_mean stats);
  check_optional_float "distinct point totals mean" 2.
    (Stats.distinct_point_totals_mean stats)

let suite =
  [
    ("audit to obs", `Quick, test_audit_to_obs);
    ("game goals mean", `Quick, test_game_goals_mean);
    ("win ratio", `Quick, test_win_ratio);
    ("filter by strategy", `Quick, test_by_strategy);
    ("filter by home", `Quick, test_by_home);
    ("filter by AI", `Quick, test_by_ai);
    ("goals per action", `Quick, test_goals_per_action);
    ("board entropy", `Quick, test_board_entropy);
    ("win rate ci", `Quick, test_win_rate_ci);
    ("win initial draw dependency", `Quick, test_win_initial_draw_dependency);
    ("initial draw tail win ratios", `Quick, test_initial_draw_tail_win_ratios);
    ( "championship point metrics without standings",
      `Quick,
      test_championship_point_metrics_without_standings );
    ("championship point metrics", `Quick, test_championship_point_metrics);
  ]
