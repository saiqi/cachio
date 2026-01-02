type t = (int * int) list

let create x = x

let eval curve score =
  curve
  |> List.filter (fun (s, _) -> score >= s)
  |> List.map snd |> List.fold_left max 0
