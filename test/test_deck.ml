open Cachio

module Fake_rng = struct
  type t = int list ref

  let int rng bound =
    match !rng with
    | x :: xs ->
        rng := xs;
        x mod bound
    | [] -> 0
end

let test_shuffle () =
  let rng = ref [ 3; 1; 4; 1; 5 ] in
  let deck = Deck.shuffle (module Fake_rng) rng Deck.full in
  List.iter
    (fun p ->
      let cards = Deck.find p deck in
      Alcotest.check Alcotest.int
        (Position.string_of_position p)
        (List.length (Deck.find p Deck.full))
        (List.length cards))
    Position.all_positions

let test_draw () =
  let deck, cards = Deck.draw Position.Defender 4 Deck.full in
  Alcotest.check Alcotest.int "expected length" 4 (List.length cards);
  Alcotest.check Alcotest.int "expected deck" 20
    (List.length (Deck.find Position.Defender deck))

let suite = [ ("shuffle", `Quick, test_shuffle); ("draw", `Quick, test_draw) ]
