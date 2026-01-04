type t = Defender | Midfielder | Forward

let all_positions = [ Defender; Midfielder; Forward ]

let string_of_position x =
  match x with
  | Defender -> "Defender"
  | Midfielder -> "Midfielder"
  | Forward -> "Forward"

let compare x y =
  match x with
  | Defender -> ( match y with Defender -> 0 | _ -> -1)
  | Midfielder -> (
      match y with Forward -> -1 | Midfielder -> 0 | Defender -> 1)
  | Forward -> ( match y with Forward -> 0 | _ -> 1)
