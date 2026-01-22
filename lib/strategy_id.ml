type t = Offensive | Defensive | Balanced | Optimal | Dummy

let all = [ Offensive; Defensive; Balanced; Optimal; Dummy ]

let to_string = function
  | Offensive -> "Offensive"
  | Defensive -> "Defensive"
  | Balanced -> "Balanced"
  | Optimal -> "Optimal"
  | Dummy -> "Dummy"

let equal x y =
  match (x, y) with
  | Defensive, Defensive -> true
  | Offensive, Offensive -> true
  | Balanced, Balanced -> true
  | Optimal, Optimal -> true
  | Dummy, Dummy -> true
  | _ -> false
