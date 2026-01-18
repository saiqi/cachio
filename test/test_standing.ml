open Cachio

let test () =
  let ids = List.init 3 Fun.id |> List.map Ai_id.of_int in
  let home = List.nth ids 0 in
  let away = List.nth ids 1 in
  let other = List.nth ids 2 in
  let result = Game_result.create ~home ~home_goals:1 ~away ~away_goals:0 in
  let standing = Standing.update (Standing.init ids) result in
  Alcotest.check Alcotest.int "home played" 1 (Standing.played home standing);
  Alcotest.check Alcotest.int "home wins" 1 (Standing.wins home standing);
  Alcotest.check Alcotest.int "home draws" 0 (Standing.draws home standing);
  Alcotest.check Alcotest.int "home losses" 0 (Standing.losses home standing);
  Alcotest.check Alcotest.int "home points" 3 (Standing.points home standing);
  Alcotest.check Alcotest.int "home gf" 1 (Standing.goals_for home standing);
  Alcotest.check Alcotest.int "home ga" 0 (Standing.goals_against home standing);
  Alcotest.check Alcotest.int "away played" 1 (Standing.played away standing);
  Alcotest.check Alcotest.int "away wins" 0 (Standing.wins away standing);
  Alcotest.check Alcotest.int "away draws" 0 (Standing.draws away standing);
  Alcotest.check Alcotest.int "away losses" 1 (Standing.losses away standing);
  Alcotest.check Alcotest.int "away points" 0 (Standing.points away standing);
  Alcotest.check Alcotest.int "away gf" 0 (Standing.goals_for away standing);
  Alcotest.check Alcotest.int "away ga" 1 (Standing.goals_against away standing);
  Alcotest.check Alcotest.int "other played" 0 (Standing.played other standing);
  Alcotest.check Alcotest.int "other wins" 0 (Standing.wins other standing);
  Alcotest.check Alcotest.int "other draws" 0 (Standing.draws other standing);
  Alcotest.check Alcotest.int "other losses" 0 (Standing.losses other standing);
  Alcotest.check Alcotest.int "other points" 0 (Standing.points other standing);
  Alcotest.check Alcotest.int "other gf" 0 (Standing.goals_for other standing);
  Alcotest.check Alcotest.int "other ga" 0
    (Standing.goals_against other standing)

let suite = [ ("update standing", `Quick, test) ]
