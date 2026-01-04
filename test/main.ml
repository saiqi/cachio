open Alcotest

let () =
  run "Cachio"
    [
      ("Piecewise", Test_piecewise.suite);
      ("Rules", Test_rules.suite);
      ("Bounded_int", Test_bounded_int.suite);
      ("Board", Test_board.suite);
      ("Roster", Test_roster.suite);
      ("Round", Test_round.suite);
      ("Lineup_generator", Test_lineup_generator.suite);
      ("Lineup_strategy", Test_lineup_strategy.suite);
    ]
