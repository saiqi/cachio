open Alcotest

let () =
  run "Cachio"
    [
      ("Piecewise", Test_piecewise.suite);
      ("Rules", Test_rules.suite);
      ("Bounded_int", Test_bounded_int.suite);
    ]
