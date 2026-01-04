open Cachio

let test () =
  let roster =
    List.init Rules.players_on_roster Fun.id
    |> List.map (fun i ->
        Player.create (Player_id.of_int i) Position.Defender
          (Score.of_int_exn 3))
    |> Roster.of_list
  in
  let boards = Lineup_generator.all roster in
  Alcotest.check Alcotest.bool "boards not empty" true (List.length boards > 0);
  Alcotest.check Alcotest.bool "valid boards" true
    (List.for_all Board.is_valid boards)

let suite = [ ("generate all", `Quick, test) ]
