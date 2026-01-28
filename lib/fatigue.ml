module PlayerIdSet = Set.Make (Player_id)

let apply ~players ~roster =
  let played = PlayerIdSet.of_list players in
  List.fold_left
    (fun acc p ->
      match PlayerIdSet.find_opt (Player.id p) played with
      | None -> Roster.add (Player.recovers p) acc
      | Some _ -> Roster.add (Player.fatigues p) acc)
    Roster.empty (Roster.to_list roster)
