open Cachio

let test () =
  let player_ids =
    List.init Rules.players_on_roster Fun.id |> List.map Player_id.of_int
  in
  let roster =
    player_ids
    |> List.map (fun i ->
        Player.create i Position.Defender (Score.of_int_exn 3))
    |> Roster.of_list
  in
  let p1 = List.nth player_ids 0 in
  let p2 = List.nth player_ids 1 in
  let p3 = List.nth player_ids 2 in
  let p4 = List.nth player_ids 3 in
  let p5 = List.nth player_ids 4 in
  let p6 = List.nth player_ids 5 in
  let low_board =
    Board.of_list
      [
        (p1, Row.of_int_exn 0, Column.of_int_exn 0);
        (p2, Row.of_int_exn 0, Column.of_int_exn 1);
        (p3, Row.of_int_exn 0, Column.of_int_exn 2);
        (p4, Row.of_int_exn 1, Column.of_int_exn 0);
        (p5, Row.of_int_exn 2, Column.of_int_exn 0);
        (p6, Row.of_int_exn 2, Column.of_int_exn 1);
      ]
  in
  let high_board =
    Board.of_list
      [
        (p1, Row.of_int_exn 0, Column.of_int_exn 0);
        (p2, Row.of_int_exn 0, Column.of_int_exn 1);
        (p3, Row.of_int_exn 1, Column.of_int_exn 2);
        (p4, Row.of_int_exn 1, Column.of_int_exn 0);
        (p5, Row.of_int_exn 2, Column.of_int_exn 0);
        (p6, Row.of_int_exn 2, Column.of_int_exn 1);
      ]
  in
  let generator _ _ _ = [ low_board; high_board ] in
  let strategy =
    Lineup_strategy.make Strategy_id.Dummy (fun b _ ->
        if b = high_board then 1 else 0)
  in
  let rng = ref [] in
  let best =
    Lineup_strategy.build
      (module Fake_rng)
      rng ~generate:generator strategy roster
  in
  let expected =
    [ (0, 0, 0); (1, 0, 1); (2, 1, 2); (3, 1, 0); (4, 2, 0); (5, 2, 1) ]
    |> List.sort compare
  in
  let result =
    Board.to_list best
    |> List.map (fun (p, r, c) ->
        (Player_id.to_int p, Row.to_int r, Column.to_int c))
    |> List.sort compare
  in
  let placement = Alcotest.(triple Alcotest.int Alcotest.int Alcotest.int) in
  Alcotest.check (Alcotest.list placement) "high board selected" expected result

let suite = [ ("select best board", `Quick, test) ]
