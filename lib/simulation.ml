type t = { participants : Participants.t; schedule : Schedule.t }

let of_ais ais =
  let participants =
    List.fold_left
      (fun acc ai -> Participants.add ai acc)
      Participants.empty ais
  in
  let schedule = Schedule.round_robin (List.map Ai.id ais) in
  { participants; schedule }

let create (type a) (module R : Rng.S with type t = a) (rng : a) ids =
  let deck = Deck.full |> Deck.shuffle (module R) rng in
  let draw_roster deck =
    let n_cards = Rules.players_on_roster / 3 in
    let deck, defenders = Deck.draw Position.Defender n_cards deck in
    let deck, midfielders = Deck.draw Position.Midfielder n_cards deck in
    let deck, forwards = Deck.draw Position.Forward n_cards deck in
    let all_cards = defenders @ midfielders @ forwards in
    (deck, Roster.of_cards all_cards)
  in
  let rec build l deck acc =
    match l with
    | [] -> List.rev acc
    | x :: xs ->
        let id, strategy = x in
        let deck, roster = draw_roster deck in
        let ai = Ai.create id roster strategy in
        build xs deck (ai :: acc)
  in
  build ids deck [] |> of_ais

let run (type a) (module R : Rng.S with type t = a) (rng : a) sim =
  League.run_with_audit (module R) rng sim.participants sim.schedule

let run_n n (type a) (module R : Rng.S with type t = a) (rng : a) sim =
  let rec aux k acc =
    if k <= 0 then List.rev acc
    else
      let res = run (module R) rng sim in
      aux (k - 1) (res :: acc)
  in
  aux n []
