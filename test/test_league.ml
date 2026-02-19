open Cachio

let fake_ais () =
  let dr =
    List.init 12 Fun.id |> List.map Player_id.of_int
    |> List.map (fun i ->
        Player.create i Position.Defender (Score.of_int_exn 1))
    |> Roster.of_list
  in
  let d =
    Ai.create (Ai_id.of_int 0) dr Strategy_id.Defensive |> Participant.of_ai
  in
  let fr =
    List.init 12 Fun.id
    |> List.map (fun i -> Player_id.of_int (i + 12))
    |> List.map (fun i -> Player.create i Position.Forward (Score.of_int_exn 3))
    |> Roster.of_list
  in
  let o =
    Ai.create (Ai_id.of_int 1) fr Strategy_id.Offensive |> Participant.of_ai
  in
  [ o; d ]

let test () =
  let ais = fake_ais () in
  let ids = List.map Participant.id ais in
  let home_id = List.nth ids 0 in
  let away_id = List.nth ids 1 in
  let rng = ref [ 1; 2; 3; 5 ] in
  let participants = Participants.of_list ais in
  let schedule =
    Schedule.of_list [ [ (home_id, away_id); (away_id, home_id) ] ]
  in
  let participants', standing, audits =
    League.run_with_audit (module Fake_rng) rng participants schedule
  in
  Alcotest.check
    (Alcotest.pair Alcotest.int Alcotest.int)
    "check game played" (2, 2)
    (Standing.played home_id standing, Standing.played away_id standing);
  Alcotest.check Alcotest.int "audit length" 2 (List.length audits);
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "participant ids" [ 0; 1 ]
    (Participants.to_list participants'
    |> List.map (fun a -> Ai_id.to_int (Participant.id a))
    |> List.sort Int.compare);
  Alcotest.check Alcotest.bool "shape decreased" true
    (List.exists
       (fun p -> Shape.compare (Player.shape p) Shape.max < 0)
       (Participants.find home_id participants'
       |> Participant.roster |> Option.get |> Roster.to_list))

let make_roster id_offset =
  let mk i pos =
    Player.create (Player_id.of_int (id_offset + i)) pos (Score.of_int_exn 3)
  in
  Roster.of_list
    [
      mk 0 Position.Defender;
      mk 1 Position.Defender;
      mk 2 Position.Midfielder;
      mk 3 Position.Midfielder;
      mk 4 Position.Forward;
      mk 5 Position.Forward;
    ]

let test_threads_participant_state_between_games () =
  let home =
    Ai.create (Ai_id.of_int 0) (make_roster 0) Strategy_id.Balanced
    |> Participant.of_ai
  in
  let away =
    Ai.create (Ai_id.of_int 1) (make_roster 100) Strategy_id.Balanced
    |> Participant.of_ai
  in
  let home_id = Participant.id home in
  let away_id = Participant.id away in
  let participants = Participants.of_list [ home; away ] in
  let schedule =
    Schedule.of_list [ [ (home_id, away_id) ]; [ (away_id, home_id) ] ]
  in
  let rng = Fake_rng.create () in
  let participants', _, audits =
    League.run_with_audit (module Fake_rng) rng participants schedule
  in
  let final_shapes id =
    Participants.find id participants'
    |> Participant.roster |> Option.get |> Roster.to_list
    |> List.map (fun p -> Shape.to_int (Player.shape p))
    |> List.sort Int.compare
  in
  Alcotest.check Alcotest.int "two games played" 2 (List.length audits);
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "home players fatigued twice (4 -> 2)" [ 2; 2; 2; 2; 2; 2 ]
    (final_shapes home_id);
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "away players fatigued twice (4 -> 2)" [ 2; 2; 2; 2; 2; 2 ]
    (final_shapes away_id)

let suite =
  [
    ("run", `Quick, test);
    ( "threads participant state",
      `Quick,
      test_threads_participant_state_between_games );
  ]
