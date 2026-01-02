open Cachio

let test () =
  let curve = Piecewise.of_list [ (0, 1); (10, 2); (20, 3); (30, 4) ] in
  let cases =
    [ (5, 1); (10, 2); (15, 2); (20, 3); (25, 3); (30, 4); (35, 4) ]
  in
  List.iter
    (fun (input, expected) ->
      let result = Piecewise.eval curve input in
      Alcotest.check Alcotest.int
        ("piecewise " ^ string_of_int input)
        expected result)
    cases

let suite = [ ("piecewise", `Quick, test) ]
