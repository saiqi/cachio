open Cachio

let test_take () =
  let input = [ 1; 2; 3; 4 ] in
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "list equals" [ 1; 2 ] (Utils.take 2 input)

let suite = [ ("take", `Quick, test_take) ]
