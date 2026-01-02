type cell = Empty | Occupied of Player_id.t

module Pos = struct
  type t = Row.t * Column.t

  let compare = Stdlib.compare
end

module PosMap = Map.Make (Pos)

type t = cell PosMap.t

let empty = PosMap.empty

let get m row col =
  match PosMap.find_opt (row, col) m with
  | None -> failwith "Not found"
  | Some v -> v

let get_opt m row col = PosMap.find_opt (row, col) m

let place i row col m =
  match get_opt m row col with
  | None -> PosMap.add (row, col) (Occupied i) m
  | Some _ -> failwith "board cell is occupied"

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

let row pos =
  match pos with
  | Position.Defender -> Row.of_int_exn 0
  | Position.Midfielder -> Row.of_int_exn 1
  | Position.Forward -> Row.of_int_exn 2
