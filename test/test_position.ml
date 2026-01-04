open Cachio

let test_compare () =
  let cases =
    [
      (Position.Defender, Position.Defender, 0);
      (Position.Defender, Position.Midfielder, -1);
      (Position.Defender, Position.Forward, -1);
      (Position.Midfielder, Position.Defender, 1);
      (Position.Midfielder, Position.Midfielder, 0);
      (Position.Midfielder, Position.Forward, -1);
      (Position.Forward, Position.Defender, 1);
      (Position.Forward, Position.Midfielder, 1);
      (Position.Forward, Position.Forward, 0);
    ]
  in
  List.iter
    (fun (x, y, exp) ->
      Alcotest.check Alcotest.int
        (Position.string_of_position x ^ " " ^ Position.string_of_position y)
        exp (Position.compare x y))
    cases

let suite = [ ("compare", `Quick, test_compare) ]
