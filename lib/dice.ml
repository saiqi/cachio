type roll = Dice_count.t -> int

let max = 6

let roll n =
  List.init (Dice_count.to_int n) (fun _ -> 1 + Random.int max)
  |> List.fold_left ( + ) 0
