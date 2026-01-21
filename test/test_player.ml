open Cachio

let test_adjust_player_score () =
  let def =
    Player.create (Player_id.of_int 0) Position.Defender (Score.of_int_exn 2)
  in
  let mid =
    Player.create (Player_id.of_int 1) Position.Midfielder (Score.of_int_exn 2)
  in
  let fwd =
    Player.create (Player_id.of_int 2) Position.Forward (Score.of_int_exn 2)
  in
  let cases =
    [
      ( def,
        [
          (Position.Defender, 2); (Position.Midfielder, 1); (Position.Forward, 1);
        ] );
      ( mid,
        [
          (Position.Defender, 1); (Position.Midfielder, 2); (Position.Forward, 1);
        ] );
      ( fwd,
        [
          (Position.Defender, 1); (Position.Midfielder, 1); (Position.Forward, 2);
        ] );
    ]
  in
  List.iter
    (fun (player, subcases) ->
      List.iter
        (fun (pos, expected) ->
          let result = Player.adjust_score player pos in
          let label =
            "player "
            ^ (Player.id player |> Player_id.to_int |> string_of_int)
            ^ " / "
            ^ Position.to_string (Player.pos player)
          in
          Alcotest.check Alcotest.int label expected
            (Score.to_int (Player.score result)))
        subcases)
    cases

let test_is_player_injured () =
  let p =
    Player.create (Player_id.of_int 0) Position.Defender (Score.of_int_exn 2)
  in
  let p1 = Player.decr_shape p in
  let p2 = Player.decr_shape p1 in
  let p3 = Player.decr_shape p2 in
  let cases = [ (p, false); (p1, false); (p2, false); (p3, true) ] in
  List.iter
    (fun (player, expected) ->
      let result = Player.is_injured player in
      let label = Player.shape player |> Shape.to_int |> string_of_int in
      Alcotest.check Alcotest.bool label expected result)
    cases

let suite =
  [
    ("adjust player score", `Quick, test_adjust_player_score);
    ("is player injured", `Quick, test_is_player_injured);
  ]
