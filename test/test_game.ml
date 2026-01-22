open Cachio

let fake_ais () =
  let dr =
    List.init 12 Fun.id |> List.map Player_id.of_int
    |> List.map (fun i ->
        Player.create i Position.Defender (Score.of_int_exn 1))
    |> Roster.of_list
  in
  let d = Ai.create (Ai_id.of_int 0) dr Lineup_strategy.defense in
  let fr =
    List.init 12 Fun.id
    |> List.map (fun i -> Player_id.of_int (i + 12))
    |> List.map (fun i -> Player.create i Position.Forward (Score.of_int_exn 3))
    |> Roster.of_list
  in
  let o = Ai.create (Ai_id.of_int 1) fr Lineup_strategy.offense in
  (o, d)

let test_home_advantage () =
  let rng = ref [ 0; 0; 0; 0; 0; 0 ] in
  let o, d = fake_ais () in
  let audit = Game.play_with_audit (module Fake_rng) rng ~home:o ~away:d in
  let home_goals = Game_audit.result audit |> Game_result.home_goals in
  let away_goals = Game_audit.result audit |> Game_result.away_goals in
  Alcotest.check
    (Alcotest.pair Alcotest.int Alcotest.int)
    "id pair" (1, 0)
    ( Ai_id.to_int (Game_result.home (Game_audit.result audit)),
      Ai_id.to_int (Game_result.away (Game_audit.result audit)) );
  Alcotest.check Alcotest.bool "home won" true (home_goals > away_goals);
  Alcotest.check Alcotest.bool "consitent offensive dice" true
    (Game_audit.home_offensive_dice audit > Game_audit.away_offensive_dice audit);
  Alcotest.check Alcotest.bool "consitent defensive dice" true
    (Game_audit.home_defensive_dice audit = Game_audit.away_defensive_dice audit);
  Alcotest.check Alcotest.bool "consistent actions" true
    (Game_audit.home_actions audit > Game_audit.away_actions audit);
  Alcotest.check Alcotest.bool "consistent home strategy" true
    (Game_audit.home_strategy audit = Strategy_id.Offensive);
  Alcotest.check Alcotest.bool "consistent away strategy" true
    (Game_audit.away_strategy audit = Strategy_id.Defensive)

let suite = [ ("home advantage", `Quick, test_home_advantage) ]
