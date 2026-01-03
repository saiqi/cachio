open Cachio

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

let suite =
  [
    ("compute param happy path", `Quick, test_compute_param_happy_path);
    ("compute param empty", `Quick, test_compute_param_empty);
  ]
