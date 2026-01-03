open Cachio

let test_place () =
  let player_id = Player_id.of_int 0 in
  let row = Row.of_int_exn 0 in
  let col = Column.of_int_exn 0 in
  let board = Board.place player_id row col Board.empty in
  match Board.get board row col with
  | Board.Occupied i ->
      Alcotest.check Alcotest.int "occupied cell" (Player_id.to_int i) 0
  | Board.Empty -> Alcotest.fail "empty cell"

let test_player_on_rows () =
  let player_ids = List.init 3 Fun.id in
  let cases =
    List.map
      (fun i -> (Player_id.of_int i, Row.of_int_exn i, Column.of_int_exn 0))
      player_ids
  in
  let board = Board.of_list cases in
  List.iter
    (fun (i, r, _) ->
      let result = Board.player_on_rows board r |> List.map Player_id.to_int in
      let msg = "row " ^ string_of_int (Row.to_int r) in
      Alcotest.check
        (Alcotest.list Alcotest.int)
        msg result
        [ Player_id.to_int i ])
    cases

let test_is_valid () =
  let pick_pos pid =
    let i = Player_id.to_int pid in
    if i < 3 then Position.Defender
    else if i < 7 then Position.Midfielder
    else Position.Forward
  in
  let player_ids = List.init 12 Fun.id |> List.map Player_id.of_int in
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
  Alcotest.check Alcotest.bool "is board valid" true (Board.is_valid board)

let test_is_not_valid () =
  let board =
    Board.place (Player_id.of_int 0) (Row.of_int_exn 0) (Column.of_int_exn 0)
      Board.empty
  in
  Alcotest.check Alcotest.bool "is board not valid" false (Board.is_valid board)

let suite =
  [
    ("place player", `Quick, test_place);
    ("players on row", `Quick, test_player_on_rows);
    ("is valid", `Quick, test_is_valid);
    ("is not valid", `Quick, test_is_not_valid);
  ]
