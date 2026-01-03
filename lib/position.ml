type t = Defender | Midfielder | Forward

let all_positions = [ Defender; Midfielder; Forward ]

let string_of_position x =
  match x with
  | Defender -> "Defender"
  | Midfielder -> "Midfielder"
  | Forward -> "Forward"
