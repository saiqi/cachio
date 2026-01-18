module M = Map.Make (Ai_id)

type entry = {
  played : int;
  wins : int;
  draws : int;
  losses : int;
  points : int;
  goals_for : int;
  goals_against : int;
}

type t = entry M.t

let empty = M.empty

let init ids =
  List.fold_left
    (fun acc i ->
      M.add i
        {
          played = 0;
          wins = 0;
          draws = 0;
          losses = 0;
          points = 0;
          goals_for = 0;
          goals_against = 0;
        }
        acc)
    M.empty ids

let update_entry ~scored ~conceded entry =
  let win = scored > conceded in
  let draw = scored = conceded in
  let loss = scored < conceded in
  {
    played = entry.played + 1;
    wins = (entry.wins + if win then 1 else 0);
    draws = (entry.draws + if draw then 1 else 0);
    losses = (entry.losses + if loss then 1 else 0);
    points = (entry.points + if win then 3 else if draw then 1 else 0);
    goals_for = entry.goals_for + scored;
    goals_against = entry.goals_against + conceded;
  }

let update standing result =
  standing
  |> M.update (Game_result.home result) (function
    | None -> assert false
    | Some entry ->
        Some
          (update_entry
             ~scored:(Game_result.home_goals result)
             ~conceded:(Game_result.away_goals result)
             entry))
  |> M.update (Game_result.away result) (function
    | None -> assert false
    | Some entry ->
        Some
          (update_entry
             ~scored:(Game_result.away_goals result)
             ~conceded:(Game_result.home_goals result)
             entry))

let played id standing = (M.find id standing).played
let wins id standing = (M.find id standing).wins
let draws id standing = (M.find id standing).draws
let losses id standing = (M.find id standing).losses
let points id standing = (M.find id standing).points
let goals_for id standing = (M.find id standing).goals_for
let goals_against id standing = (M.find id standing).goals_against
