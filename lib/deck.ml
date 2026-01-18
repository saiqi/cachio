module PosMap = Map.Make (Position)

type t = Card.t list PosMap.t

let empty = PosMap.empty

let full =
  [
    (Position.Defender, Card_pool.defenders);
    (Position.Midfielder, Card_pool.midfielders);
    (Position.Forward, Card_pool.forwards);
  ]
  |> List.fold_left (fun acc (p, l) -> PosMap.add p l acc) empty

let find position deck = PosMap.find position deck

let shuffle (type a) (module R : Rng.S with type t = a) (rng : a) deck =
  Position.all_positions
  |> List.map (fun p -> (p, PosMap.find p deck))
  |> List.fold_left
       (fun acc (p, l) -> PosMap.add p (Utils.shuffle (module R) rng l) acc)
       deck

let draw position n deck =
  let rest, taken = Utils.draw n (PosMap.find position deck) in
  (PosMap.add position rest deck, taken)
