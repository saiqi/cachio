type t = Offensive | Defensive | Balanced | Optimal | Dummy | Pragmatic

let all = [ Offensive; Defensive; Balanced; Optimal; Dummy; Pragmatic ]

let to_string = function
  | Offensive -> "Offensive"
  | Defensive -> "Defensive"
  | Balanced -> "Balanced"
  | Optimal -> "Optimal"
  | Dummy -> "Dummy"
  | Pragmatic -> "Pragmatic"
