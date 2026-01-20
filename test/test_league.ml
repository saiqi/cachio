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
  [ o; d ]

let test () =
  let ais = fake_ais () in
  let ids = List.map Ai.id ais in
  let home_id = List.nth ids 0 in
  let away_id = List.nth ids 1 in
  let rng = ref [ 1; 2; 3; 5 ] in
  let participants = Participants.of_list ais in
  let schedule =
    Schedule.of_list [ [ (home_id, away_id); (away_id, home_id) ] ]
  in
  let standing = League.run (module Fake_rng) rng participants schedule in
  Alcotest.check
    (Alcotest.pair Alcotest.int Alcotest.int)
    "check game played" (2, 2)
    (Standing.played home_id standing, Standing.played away_id standing)

let suite = [ ("expected standing", `Quick, test) ]
