open Cachio

module Dummy_int = Bounded_int.Make (struct
  let min = 1
  let max = 6
end)

let test_arithmetic () =
  let x = Dummy_int.of_int_exn 6 in
  let y = Dummy_int.of_int_exn 3 in
  let r = Dummy_int.add (Dummy_int.incr x) (Dummy_int.decr y) in
  Alcotest.check Alcotest.int "bounded arithmetic" 8 r

let test_all () =
  let all_values = Dummy_int.all |> List.map Dummy_int.to_int in
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "all values" [ 1; 2; 3; 4; 5; 6 ] all_values

let suite =
  [ ("arithmetic", `Quick, test_arithmetic); ("all values", `Quick, test_all) ]
