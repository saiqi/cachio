open Cachio

let test_forwards () =
  Alcotest.check Alcotest.int "score equals" 24 (List.length Card_pool.forwards)

let test_midfielders () =
  Alcotest.check Alcotest.int "score equals" 24
    (List.length Card_pool.midfielders)

let test_defenders () =
  Alcotest.check Alcotest.int "score equals" 24
    (List.length Card_pool.defenders)

let suite =
  [
    ("count forward cards", `Quick, test_forwards);
    ("count midfielder cards", `Quick, test_midfielders);
    ("count defender cards", `Quick, test_defenders);
  ]
