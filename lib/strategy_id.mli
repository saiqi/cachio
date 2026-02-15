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

val all : t list
val to_string : t -> string
val is_scripted : t -> bool
