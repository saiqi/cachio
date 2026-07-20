let of_ais ais =
  let participants =
    List.fold_left
      (fun acc ai -> Participants.add ai acc)
      Participants.empty ais
  in
  let schedule = Schedule.round_robin (List.map Participant.id ais) in
  (participants, schedule)

let create (type a) (module R : Rng.S with type t = a) (rng : a) ids =
  let deck = Deck.full |> Deck.shuffle (module R) rng in
  let n_cards = Rules.players_on_roster / 3 in
  let scripted, playable =
    List.partition (fun (_, strategy) -> Strategy_id.is_scripted strategy) ids
  in
  let pick_best cards =
    match cards with
    | [] -> failwith "Simulation.create: not enough cards to draft"
    | first :: rest ->
        let first_score = Card.score first |> Score.to_int in
        let best, _, others =
          List.fold_left
            (fun (best, best_score, others) card ->
              let score = Card.score card |> Score.to_int in
              if score > best_score then (card, score, best :: others)
              else (best, best_score, card :: others))
            (first, first_score, []) rest
        in
        (best, List.rev others)
  in
  let rotate n l =
    let len = List.length l in
    if len = 0 then l
    else
      let n = n mod len in
      let rec split k left right =
        if k = 0 then (List.rev left, right)
        else
          match right with
          | [] -> (List.rev left, [])
          | x :: xs -> split (k - 1) (x :: left) xs
      in
      let left, right = split n [] l in
      right @ left
  in
  let module AiMap = Map.Make (Ai_id) in
  let initial_cards =
    List.fold_left (fun acc (id, _) -> AiMap.add id [] acc) AiMap.empty playable
  in
  let base_order = playable |> Utils.shuffle (module R) rng in
  let draft_position pos_index rosters position =
    let initial_order = rotate pos_index base_order in
    let rec draft_round round cards rosters =
      if round >= n_cards then rosters
      else
        let order =
          if round mod 2 = 0 then initial_order else List.rev initial_order
        in
        let cards, rosters =
          List.fold_left
            (fun (cards, rosters) (id, _) ->
              let card, cards = pick_best cards in
              let rosters =
                AiMap.update id
                  (function
                    | None -> Some [ card ] | Some cards -> Some (card :: cards))
                  rosters
              in
              (cards, rosters))
            (cards, rosters) order
        in
        draft_round (round + 1) cards rosters
    in
    draft_round 0 (Deck.find position deck) rosters
  in
  let roster_cards =
    Position.all
    |> List.mapi (fun i position -> (i, position))
    |> List.fold_left
         (fun rosters (i, position) -> draft_position i rosters position)
         initial_cards
  in
  let scripted_participants =
    List.map
      (fun (id, strategy) ->
        Scripted.create id strategy |> Participant.of_scripted)
      scripted
  in
  let ai_participants =
    List.map
      (fun (id, strategy) ->
        let cards = AiMap.find id roster_cards in
        Ai.create id (Roster.of_cards cards) strategy |> Participant.of_ai)
      playable
  in
  scripted_participants @ ai_participants |> of_ais

let run (type a) (module R : Rng.S with type t = a) (rng : a) ids =
  let participants, schedule = create (module R) rng ids in
  let _, standing, audit =
    League.run_with_audit (module R) rng participants schedule
  in
  (standing, audit)

let run_n n (type a) (module R : Rng.S with type t = a) (rng : a) ids =
  let rec aux k acc =
    if k <= 0 then List.rev acc
    else
      let res = run (module R) rng ids in
      aux (k - 1) (res :: acc)
  in
  aux n []
