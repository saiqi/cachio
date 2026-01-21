type t = Offensive | Defensive | Balanced | Optimal

let all = [ Offensive; Defensive; Balanced; Optimal ]

let to_string = function
  | Offensive -> "Offensive"
  | Defensive -> "Defensive"
  | Balanced -> "Balanced"
  | Optimal -> "Optimal"
