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
  let result = Game.play (module Fake_rng) rng ~home:o ~away:d in
  Alcotest.check
    (Alcotest.pair Alcotest.int Alcotest.int)
    "id pair" (1, 0)
    ( Ai_id.to_int (Game_result.home result),
      Ai_id.to_int (Game_result.away result) );
  print_endline (Game_result.home_goals result |> string_of_int);
  print_endline (Game_result.away_goals result |> string_of_int);
  Alcotest.check Alcotest.bool "home won" true
    (Game_result.home_goals result > Game_result.away_goals result)

let suite = [ ("home advantage", `Quick, test_home_advantage) ]
