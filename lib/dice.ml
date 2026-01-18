let max = 6

let roll (type a) (module R : Rng.S with type t = a) (rng : a) n =
  List.init (Dice_count.to_int n) (fun _ -> 1 + (R.int rng) max)
  |> List.fold_left ( + ) 0
