open Cachio

let create_deterministic_ais (type a) (module R : Rng.S with type t = a)
    (rng : a) =
  let deck = Deck.full |> Deck.shuffle (module R) rng in
  let draw_roster deck =
    let n_cards = Rules.players_on_roster / 3 in
    let deck, defenders = Deck.draw Position.Defender n_cards deck in
    let deck, midfielders = Deck.draw Position.Midfielder n_cards deck in
    let deck, forwards = Deck.draw Position.Forward n_cards deck in
    let all_cards = defenders @ midfielders @ forwards in
    (deck, Roster.of_cards all_cards)
  in
  let rec build n deck acc =
    if n = 0 then List.rev acc
    else
      let id = Ai_id.of_int n in
      let deck, roster = draw_roster deck in
      let strategy =
        if n <= 2 then Strategy_id.Defensive
        else if n <= 4 then Strategy_id.Balanced
        else Strategy_id.Offensive
      in
      let ai = Ai.create id roster strategy in
      build (n - 1) deck (ai :: acc)
  in
  build 6 deck []
