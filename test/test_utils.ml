open Cachio

let test_draw () =
  let input = [ 1; 2; 3; 4 ] in
  let rest, taken = Utils.draw 2 input in
  Alcotest.check (Alcotest.list Alcotest.int) "list equals" [ 1; 2 ] taken;
  Alcotest.check (Alcotest.list Alcotest.int) "list equals" [ 3; 4 ] rest

let test_take () =
  let input = [ 1; 2; 3; 4 ] in
  let taken = Utils.take 2 input in
  Alcotest.check (Alcotest.list Alcotest.int) "list equals" [ 1; 2 ] taken

let test_drop () =
  let input = [ 1; 2; 3; 4 ] in
  let dropped = Utils.drop 2 input in
  Alcotest.check (Alcotest.list Alcotest.int) "list equals" [ 3; 4 ] dropped

let suite =
  [
    ("draw", `Quick, test_draw);
    ("take", `Quick, test_take);
    ("drop", `Quick, test_drop);
  ]
