open Cachio

let player_id =
  Alcotest.testable
    (fun fmt x -> Format.pp_print_int fmt (Player_id.to_int x))
    Player_id.equal

let row =
  Alcotest.testable
    (fun fmt r -> Format.pp_print_int fmt (Row.to_int r))
    Row.equal

let column =
  Alcotest.testable
    (fun fmt c -> Format.pp_print_int fmt (Column.to_int c))
    Column.equal

let placement = Alcotest.(triple player_id row column)

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

let test_players () =
  let player_ids = List.init 3 Fun.id in
  let cases =
    List.map
      (fun i -> (Player_id.of_int i, Row.of_int_exn i, Column.of_int_exn 0))
      player_ids
  in
  let board = Board.of_list cases in
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "same list" player_ids
    (Board.players board
    |> List.sort Player_id.compare
    |> List.map Player_id.to_int)

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

let test_to_list () =
  let l =
    Board.place (Player_id.of_int 0) (Row.of_int_exn 0) (Column.of_int_exn 0)
      Board.empty
    |> Board.to_list
  in
  Alcotest.check
    Alcotest.(list placement)
    "same list"
    [ (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 0) ]
    l

let test_count () =
  let board =
    Board.of_list
      [
        (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 0);
        (Player_id.of_int 1, Row.of_int_exn 0, Column.of_int_exn 1);
        (Player_id.of_int 2, Row.of_int_exn 0, Column.of_int_exn 2);
        (Player_id.of_int 3, Row.of_int_exn 1, Column.of_int_exn 0);
        (Player_id.of_int 4, Row.of_int_exn 1, Column.of_int_exn 1);
      ]
  in
  Alcotest.check Alcotest.int "expected count" 5 (Board.count board)

let test_can_place () =
  Alcotest.check Alcotest.bool "can place on empty" true
    (Board.can_place Board.empty (Player_id.of_int 5) (Row.of_int_exn 0)
       (Column.of_int_exn 0));
  let board =
    Board.of_list
      [
        (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 0);
        (Player_id.of_int 1, Row.of_int_exn 0, Column.of_int_exn 1);
        (Player_id.of_int 2, Row.of_int_exn 0, Column.of_int_exn 2);
        (Player_id.of_int 3, Row.of_int_exn 1, Column.of_int_exn 0);
        (Player_id.of_int 4, Row.of_int_exn 1, Column.of_int_exn 1);
      ]
  in
  Alcotest.check Alcotest.bool "can place on offense" true
    (Board.can_place board (Player_id.of_int 5) (Row.of_int_exn 2)
       (Column.of_int_exn 0));
  Alcotest.check Alcotest.bool "cannot place on midfield" false
    (Board.can_place board (Player_id.of_int 5) (Row.of_int_exn 1)
       (Column.of_int_exn 2));
  Alcotest.check Alcotest.bool "cannot place on defense" false
    (Board.can_place board (Player_id.of_int 5) (Row.of_int_exn 0)
       (Column.of_int_exn 3))

let test_is_shape_valid () =
  let l =
    [
      (Row.of_int_exn 0, Column.of_int_exn 0);
      (Row.of_int_exn 0, Column.of_int_exn 1);
      (Row.of_int_exn 1, Column.of_int_exn 0);
      (Row.of_int_exn 1, Column.of_int_exn 1);
      (Row.of_int_exn 2, Column.of_int_exn 0);
      (Row.of_int_exn 2, Column.of_int_exn 1);
    ]
  in
  Alcotest.check Alcotest.bool "shape is valid" true (Board.is_shape_valid l)

let test_is_shape_not_valid () =
  let l =
    [
      (Row.of_int_exn 0, Column.of_int_exn 0);
      (Row.of_int_exn 0, Column.of_int_exn 1);
      (Row.of_int_exn 0, Column.of_int_exn 2);
      (Row.of_int_exn 0, Column.of_int_exn 3);
      (Row.of_int_exn 2, Column.of_int_exn 0);
      (Row.of_int_exn 2, Column.of_int_exn 1);
    ]
  in
  Alcotest.check Alcotest.bool "shape is not valid" false
    (Board.is_shape_valid l)

let test_hash () =
  let values =
    [
      (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 0);
      (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 1);
      (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 2);
      (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 3);
      (Player_id.of_int 0, Row.of_int_exn 1, Column.of_int_exn 0);
      (Player_id.of_int 0, Row.of_int_exn 2, Column.of_int_exn 0);
    ]
  in
  let b1 = Board.of_list values in
  let b2 = Board.of_list values in
  Alcotest.check Alcotest.bool "hash equals" true (Board.hash b1 = Board.hash b2)

let test_rotate () =
  let rotated_board =
    Board.of_list
      [
        (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 0);
        (Player_id.of_int 1, Row.of_int_exn 0, Column.of_int_exn 1);
        (Player_id.of_int 2, Row.of_int_exn 0, Column.of_int_exn 2);
        (Player_id.of_int 3, Row.of_int_exn 0, Column.of_int_exn 3);
        (Player_id.of_int 4, Row.of_int_exn 1, Column.of_int_exn 0);
        (Player_id.of_int 5, Row.of_int_exn 2, Column.of_int_exn 0);
      ]
    |> Board.rotate
  in
  let sort_placement (p1, _, _) (p2, _, _) = Player_id.compare p1 p2 in
  Alcotest.check (Alcotest.list placement) "rotate 180"
    [
      (Player_id.of_int 0, Row.of_int_exn 0, Column.of_int_exn 3);
      (Player_id.of_int 1, Row.of_int_exn 0, Column.of_int_exn 2);
      (Player_id.of_int 2, Row.of_int_exn 0, Column.of_int_exn 1);
      (Player_id.of_int 3, Row.of_int_exn 0, Column.of_int_exn 0);
      (Player_id.of_int 4, Row.of_int_exn 1, Column.of_int_exn 3);
      (Player_id.of_int 5, Row.of_int_exn 2, Column.of_int_exn 3);
    ]
    (Board.to_list rotated_board |> List.sort sort_placement)

let suite =
  [
    ("place player", `Quick, test_place);
    ("players on row", `Quick, test_player_on_rows);
    ("is valid", `Quick, test_is_valid);
    ("is not valid", `Quick, test_is_not_valid);
    ("to list", `Quick, test_to_list);
    ("count", `Quick, test_count);
    ("can place", `Quick, test_can_place);
    ("is shape valid", `Quick, test_is_shape_valid);
    ("is shape not valid", `Quick, test_is_shape_not_valid);
    ("hash", `Quick, test_hash);
    ("rotate", `Quick, test_rotate);
  ]
