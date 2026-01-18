open Cachio

let test_compute_postion_score () =
  let positions = Position.all_positions in
  List.iter
    (fun p ->
      let player = Player.create (Player_id.of_int 0) p (Score.of_int_exn 3) in
      let roster = Roster.add player Roster.empty in
      let board =
        Board.place (Player.id player) (Board.row p) (Column.of_int_exn 0)
          Board.empty
      in
      Alcotest.check Alcotest.int
        (Position.string_of_position p)
        3
        (Round.compute_position_score ~board ~roster ~position:p))
    positions

let test_compute_param_happy_path () =
  let score = Score.of_int_exn 3 in
  let pick_pos pid =
    let i = Player_id.to_int pid in
    if i < 3 then Position.Defender
    else if i < 7 then Position.Midfielder
    else Position.Forward
  in
  let player_ids = List.init 12 Fun.id |> List.map Player_id.of_int in
  let players =
    player_ids |> List.map (fun i -> Player.create i (pick_pos i) score)
  in
  let roster = Roster.of_list players in
  let selected_ids =
    List.filter (fun x -> Player_id.to_int x mod 2 = 0) player_ids
  in
  let board =
    Board.of_list
      (List.map
         (fun i ->
           let v = Player_id.to_int i in
           let r = Board.row (pick_pos i) in
           let c =
             Column.of_int_exn (if v = 2 || v = 6 || v = 10 then 1 else 0)
           in
           (i, r, c))
         selected_ids)
  in
  let param = Round.compute_param ~home:false ~board ~roster in
  let offensive_dices =
    Round_param.offensive_dices param |> Dice_count.to_int
  in
  let defensive_dices =
    Round_param.defensive_dices param |> Dice_count.to_int
  in
  let actions = Round_param.actions param |> Action_count.to_int in
  Alcotest.check Alcotest.int "offensive dices"
    (Piecewise.eval Balance.offensive_dice_curve 6)
    offensive_dices;
  Alcotest.check Alcotest.int "defensive dices"
    (Piecewise.eval Balance.defensive_dice_curve 6)
    defensive_dices;
  Alcotest.check Alcotest.int "actions"
    (Piecewise.eval Balance.action_curve 6)
    actions

let test_compute_param_empty () =
  let param =
    Round.compute_param ~home:false ~board:Board.empty ~roster:Roster.empty
  in
  let offensive_dices =
    Round_param.offensive_dices param |> Dice_count.to_int
  in
  let defensive_dices =
    Round_param.defensive_dices param |> Dice_count.to_int
  in
  let actions = Round_param.actions param |> Action_count.to_int in
  Alcotest.check Alcotest.int "offensive dices" 1 offensive_dices;
  Alcotest.check Alcotest.int "defensive dices" 1 defensive_dices;
  Alcotest.check Alcotest.int "actions" 1 actions

let test_resolve () =
  let home =
    Round_param.create ~offensive_dices:(Dice_count.of_int_exn 2)
      ~defensive_dices:(Dice_count.of_int_exn 2)
      ~actions:(Action_count.of_int_exn 2)
  in
  let away =
    Round_param.create ~offensive_dices:(Dice_count.of_int_exn 1)
      ~defensive_dices:(Dice_count.of_int_exn 1)
      ~actions:(Action_count.of_int_exn 1)
  in
  let rolls = ref [ 5; 3; 2; 5; 0; 0; 0; 0; 3 ] in
  let home_goals, away_goals =
    Round.resolve (module Fake_rng) rolls ~home ~away
  in
  Alcotest.check Alcotest.int "home goals" 1 home_goals;
  Alcotest.check Alcotest.int "away goals" 1 away_goals

let suite =
  [
    ("compute position score", `Quick, test_compute_postion_score);
    ("compute param happy path", `Quick, test_compute_param_happy_path);
    ("compute param empty", `Quick, test_compute_param_empty);
    ("resolve", `Quick, test_resolve);
  ]
