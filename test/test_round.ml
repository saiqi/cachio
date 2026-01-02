open Cachio

let test_compute_param () =
  let score = Score.of_int_exn 3 in
  let pick_pos pid =
    let i = Player_id.to_int pid in
    if i < 3 then Position.Defender
    else if i < 7 then Position.Midfielder
    else Position.Forward
  in
  let player_ids = List.init 12 Fun.id |> List.map Player_id.of_int in
  let players =
    player_ids |> List.map (fun i -> Player.create i (pick_pos i) score)
  in
  let roster =
    List.fold_left (fun acc p -> Roster.add p acc) Roster.empty players
  in
  roster
