type cell = Empty | Occupied of Player_id.t

module Pos = struct
  type t = Row.t * Column.t

  let compare = Stdlib.compare
end

module PosMap = Map.Make (Pos)

type t = cell PosMap.t

let empty = PosMap.empty
let get m row col = PosMap.find (row, col) m

let place i row col m =
  match get m row col with
  | Empty -> PosMap.add (row, col) (Occupied i) m
  | Occupied _ -> failwith "board cell is occupied"

let remove row col m = PosMap.remove (row, col) m

let player_on_rows m row =
  PosMap.fold
    (fun (r, _c) cell acc ->
      if Row.compare r row == 0 then
        match cell with Empty -> acc | Occupied i -> i :: acc
      else acc)
    m []

let position row =
  let i = Row.to_int row in
  if i == 0 then Position.Defender
  else if i == 1 then Position.Midfielder
  else Position.Forward
