type t =
  | Offensive
  | Defensive
  | Balanced
  | Optimal
  | Dummy
  | Pragmatic
  | Scripted_Offensive
  | Scripted_Balanced
  | Scripted_Defensive

let all =
  [
    Offensive;
    Defensive;
    Balanced;
    Optimal;
    Dummy;
    Pragmatic;
    Scripted_Balanced;
    Scripted_Defensive;
    Scripted_Offensive;
  ]

let to_string = function
  | Offensive -> "Offensive"
  | Defensive -> "Defensive"
  | Balanced -> "Balanced"
  | Optimal -> "Optimal"
  | Dummy -> "Dummy"
  | Pragmatic -> "Pragmatic"
  | Scripted_Offensive -> "Scripted Offensive"
  | Scripted_Balanced -> "Scripted Balanced"
  | Scripted_Defensive -> "Scripted Defensive"

let is_scripted = function
  | Scripted_Offensive -> true
  | Scripted_Balanced -> true
  | Scripted_Defensive -> true
  | _ -> false
