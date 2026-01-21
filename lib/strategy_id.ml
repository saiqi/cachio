type t = Offensive | Defensive | Balanced | Optimal | Dummy

let all = [ Offensive; Defensive; Balanced; Optimal; Dummy ]

let to_string = function
  | Offensive -> "Offensive"
  | Defensive -> "Defensive"
  | Balanced -> "Balanced"
  | Optimal -> "Optimal"
  | Dummy -> "Dummy"
