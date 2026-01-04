module PosMap = Map.Make (Position)

type t = Card.t list PosMap.t
type shuffle = Card.t list -> Card.t list

let empty = PosMap.empty

let full =
  [
    (Position.Defender, Card_pool.defenders);
    (Position.Midfielder, Card_pool.midfielders);
    (Position.Forward, Card_pool.forwards);
  ]
  |> List.fold_left (fun acc (p, l) -> PosMap.add p l acc) empty

let find position deck = PosMap.find position deck

let shuffle shuffle deck =
  Position.all_positions
  |> List.map (fun p -> (p, PosMap.find p deck))
  |> List.fold_left (fun acc (p, l) -> PosMap.add p (shuffle l) acc) deck

let draw position n deck = Utils.take n (PosMap.find position deck)
