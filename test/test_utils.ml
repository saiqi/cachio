open Cachio

let test_take () =
  let input = [ 1; 2; 3; 4 ] in
  let rest, taken = Utils.draw 2 input in
  Alcotest.check (Alcotest.list Alcotest.int) "list equals" [ 1; 2 ] taken;
  Alcotest.check (Alcotest.list Alcotest.int) "list equals" [ 3; 4 ] rest

let suite = [ ("take", `Quick, test_take) ]
