type t = Defender | Midfielder | Forward

let all = [ Defender; Midfielder; Forward ]

let to_string = function
  | Defender -> "Defender"
  | Midfielder -> "Midfielder"
  | Forward -> "Forward"

let compare x y =
  match x with
  | Defender -> ( match y with Defender -> 0 | _ -> -1)
  | Midfielder -> (
      match y with Forward -> -1 | Midfielder -> 0 | Defender -> 1)
  | Forward -> ( match y with Forward -> 0 | _ -> 1)
