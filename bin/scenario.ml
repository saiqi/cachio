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
  let rec build n strategies deck acc =
    match strategies with
    | [] -> List.rev acc
    | x :: xs ->
        let id = Ai_id.of_int n in
        let deck, roster = draw_roster deck in
        let ai = Ai.create id roster x in
        build (n - 1) xs deck (ai :: acc)
  in
  let strategies = Strategy_id.all in
  build (List.length strategies) strategies deck []
