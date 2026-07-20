open Cachio

let fake_roster_board =
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
  (roster, board)

let test_compute_postion_score () =
  let positions = Position.all in
  List.iter
    (fun p ->
      let player = Player.create (Player_id.of_int 0) p (Score.of_int_exn 3) in
      let roster = Roster.add player Roster.empty in
      let board =
        Board.place (Player.id player) (Board.row p) (Column.of_int_exn 0)
          Board.empty
      in
      Alcotest.check Alcotest.int (Position.to_string p) 3
        (Round.compute_position_score ~board ~roster ~position:p))
    positions

let test_compute_param_happy_path () =
  let roster, board = fake_roster_board in
  let param = Round.compute_param ~home:false ~board ~roster in
  let offensive_dice = Round_param.offensive_dice param |> Dice_count.to_int in
  let defensive_dice = Round_param.defensive_dice param |> Dice_count.to_int in
  let actions = Round_param.actions param |> Action_count.to_int in
  Alcotest.check Alcotest.int "offensive dices"
    (Piecewise.eval Balance.offensive_dice_curve 6)
    offensive_dice;
  Alcotest.check Alcotest.int "defensive dices"
    (Piecewise.eval Balance.defensive_dice_curve 6)
    defensive_dice;
  Alcotest.check Alcotest.int "actions"
    (Piecewise.eval Balance.action_curve 6)
    actions

let test_compute_param_empty () =
  let param =
    Round.compute_param ~home:false ~board:Board.empty ~roster:Roster.empty
  in
  let offensive_dice = Round_param.offensive_dice param |> Dice_count.to_int in
  let defensive_dice = Round_param.defensive_dice param |> Dice_count.to_int in
  let actions = Round_param.actions param |> Action_count.to_int in
  Alcotest.check Alcotest.int "offensive dices" 1 offensive_dice;
  Alcotest.check Alcotest.int "defensive dices" 1 defensive_dice;
  Alcotest.check Alcotest.int "actions" 1 actions

let test_compute_param_line_bonus () =
  let players =
    List.init 12 (fun i ->
        let position =
          if i < 4 then Position.Defender
          else if i < 8 then Position.Forward
          else Position.Midfielder
        in
        Player.create (Player_id.of_int i) position (Score.of_int_exn 1))
  in
  let roster = Roster.of_list players in
  let board =
    Board.of_list
      [
        (Player_id.of_int 0, Board.row Position.Defender, Column.of_int_exn 0);
        (Player_id.of_int 1, Board.row Position.Defender, Column.of_int_exn 1);
        (Player_id.of_int 2, Board.row Position.Defender, Column.of_int_exn 2);
        (Player_id.of_int 3, Board.row Position.Defender, Column.of_int_exn 3);
        (Player_id.of_int 4, Board.row Position.Forward, Column.of_int_exn 0);
        (Player_id.of_int 5, Board.row Position.Forward, Column.of_int_exn 1);
      ]
  in
  let param = Round.compute_param ~home:false ~board ~roster in
  Alcotest.check Alcotest.int "offensive dice no line bonus" 1
    (Round_param.offensive_dice param |> Dice_count.to_int);
  Alcotest.check Alcotest.int "defensive dice line bonus" 2
    (Round_param.defensive_dice param |> Dice_count.to_int);
  Alcotest.check Alcotest.int "actions no line bonus" 1
    (Round_param.actions param |> Action_count.to_int)

let test_adjust_param () =
  let _, fake = fake_roster_board in
  let left = Some fake in
  let right = Option.map (fun l -> Board.of_list (Board.to_list l)) left in
  let right' = Option.map Board.rotate right in
  let left_param =
    Round_param.create ~offensive_dice:(Dice_count.of_int_exn 1)
      ~defensive_dice:(Dice_count.of_int_exn 1)
      ~actions:(Action_count.of_int_exn 1)
  in
  let right_param =
    Round_param.create ~offensive_dice:(Dice_count.of_int_exn 1)
      ~defensive_dice:(Dice_count.of_int_exn 1)
      ~actions:(Action_count.of_int_exn 1)
  in
  let left_param', right_param' =
    Round.adjust_param ~left ~left_param ~right ~right_param
  in
  let left_param'', right_param'' =
    Round.adjust_param ~left ~left_param ~right:right' ~right_param
  in
  Alcotest.check Alcotest.int "left -> not covered no change" 1
    (Round_param.defensive_dice left_param' |> Dice_count.to_int);
  Alcotest.check Alcotest.int "right -> not covered no change" 1
    (Round_param.defensive_dice right_param' |> Dice_count.to_int);
  Alcotest.check Alcotest.int "left -> covered no change" 1
    (Round_param.defensive_dice left_param'' |> Dice_count.to_int);
  Alcotest.check Alcotest.int "right -> covered no change" 1
    (Round_param.defensive_dice right_param'' |> Dice_count.to_int)

let test_resolve () =
  let home =
    Round_param.create ~offensive_dice:(Dice_count.of_int_exn 2)
      ~defensive_dice:(Dice_count.of_int_exn 2)
      ~actions:(Action_count.of_int_exn 2)
  in
  let away =
    Round_param.create ~offensive_dice:(Dice_count.of_int_exn 1)
      ~defensive_dice:(Dice_count.of_int_exn 1)
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
    ("compute param line bonus", `Quick, test_compute_param_line_bonus);
    ("adjust param", `Quick, test_adjust_param);
    ("resolve", `Quick, test_resolve);
  ]
