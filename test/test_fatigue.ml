open Cachio

let test () =
  let player_ids = List.init 12 Fun.id |> List.map Player_id.of_int in
  let players =
    player_ids
    |> List.map (fun i ->
        Player.create i Position.Defender (Score.of_int_exn 3))
  in
  let roster = Roster.of_list players in
  let played = List.filter (fun i -> Player_id.to_int i < 6) player_ids in
  let roster' = Fatigue.apply ~players:played ~roster in
  let sort_player p1 p2 = Player_id.compare (Player.id p1) (Player.id p2) in
  List.iter2
    (fun before after ->
      let before_shape = Player.shape before |> Shape.to_int in
      let after_shape = Player.shape after |> Shape.to_int in
      let has_played =
        match
          List.find_opt (fun i -> Player_id.equal (Player.id before) i) played
        with
        | None -> false
        | Some _ -> true
      in
      Alcotest.check Alcotest.bool "shape lesser or equals" true
        (if has_played then after_shape < before_shape
         else after_shape = before_shape))
    (Roster.to_list roster |> List.sort sort_player)
    (Roster.to_list roster' |> List.sort sort_player)

let suite = [ ("apply", `Quick, test) ]
