open Cachio

let test () =
  let rolls = ref [ 0; 3 ] in
  let result = Dice.roll (module Fake_rng) rolls (Dice_count.of_int_exn 2) in
  Alcotest.check Alcotest.int "expected sum" 5 result

let suite = [ ("roll", `Quick, test) ]
