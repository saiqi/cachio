open Cachio

let player =
  Alcotest.testable
    (fun fmt x -> Format.pp_print_int fmt (Player.id x |> Player_id.to_int))
    Player.equal

let test_add () =
  let player =
    Player.create (Player_id.of_int 0) Position.Defender (Score.of_int_exn 3)
  in
  let roster = Roster.add player Roster.empty in
  match Roster.find_opt (Player.id player) roster with
  | Some p ->
      Alcotest.check Alcotest.int "player id matches"
        (Player.id p |> Player_id.to_int)
        0
  | None -> Alcotest.fail "player id does not match"

let test_of_cards () =
  let cards = [ Card.create Position.Defender (Score.of_int_exn 3) ] in
  let roster = Roster.of_cards cards in
  let expected =
    [
      Player.create (Player_id.of_int 0) Position.Defender (Score.of_int_exn 3);
    ]
  in
  Alcotest.check
    Alcotest.(list player)
    "single element list" expected (Roster.to_list roster)

let suite =
  [
    ("add player", `Quick, test_add); ("roster of cards", `Quick, test_of_cards);
  ]
