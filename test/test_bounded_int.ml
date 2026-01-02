open Cachio

module Dummy_int = Bounded_int.Make (struct
  let min = 1
  let max = 6
end)

let test () =
  let x = Dummy_int.of_int_exn 6 in
  let y = Dummy_int.of_int_exn 3 in
  let r = Dummy_int.add (Dummy_int.incr x) (Dummy_int.decr y) in
  Alcotest.check Alcotest.int "test bounded arithmetic" 8 r

let suite = [ ("bounded int", `Quick, test) ]
