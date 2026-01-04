let build_list dist position =
  dist
  |> List.map (fun (score, count) ->
      List.init count (fun _ -> Card.create position (Score.of_int_exn score)))
  |> List.flatten

let defenders =
  build_list
    [ (1, 6); (2, 6); (3, 5); (4, 4); (5, 2); (6, 1) ]
    Position.Defender

let forwards =
  build_list [ (1, 2); (2, 4); (3, 5); (4, 6); (5, 5); (6, 2) ] Position.Forward

let midfielders =
  build_list
    [ (1, 4); (2, 5); (3, 6); (4, 5); (5, 3); (6, 1) ]
    Position.Midfielder
