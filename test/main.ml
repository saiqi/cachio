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
      ("Card_pool", Test_card_pool.suite);
      ("Position", Test_position.suite);
      ("Deck", Test_deck.suite);
      ("Utils", Test_utils.suite);
    ]
