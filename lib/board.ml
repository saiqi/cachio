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
  | Some _ ->
      failwith
        ("board cell is occupied ("
        ^ string_of_int (Row.to_int row)
        ^ ", "
        ^ string_of_int (Column.to_int col)
        ^ ")")

let remove row col m = PosMap.remove (row, col) m

let player_on_rows m row =
  PosMap.fold
    (fun (r, _c) cell acc ->
      if Row.compare r row == 0 then
        match cell with Empty -> acc | Occupied i -> i :: acc
      else acc)
    m []

let players board =
  PosMap.fold
    (fun (_, _) cell acc ->
      match cell with Occupied i -> i :: acc | Empty -> acc)
    board []

let position row =
  let i = Row.to_int row in
  if i = 0 then Position.Defender
  else if i = 1 then Position.Midfielder
  else Position.Forward

let row pos =
  match pos with
  | Position.Defender -> Row.of_int_exn 0
  | Position.Midfielder -> Row.of_int_exn 1
  | Position.Forward -> Row.of_int_exn 2

let of_list l = List.fold_left (fun acc (i, r, c) -> place i r c acc) empty l

let to_list m =
  PosMap.to_list m
  |> List.fold_left
       (fun acc ((r, c), cell) ->
         match cell with Empty -> acc | Occupied p -> (p, r, c) :: acc)
       []

let is_valid m =
  let rows = Position.all |> List.map row in
  List.for_all
    (fun r -> List.length (player_on_rows m r) >= Rules.min_players_on_row)
    rows

module RowCount = Map.Make (Row)

let is_shape_valid l =
  let init = RowCount.of_list (List.map (fun e -> (e, 0)) Row.all) in
  List.fold_left
    (fun acc (r, _) ->
      match RowCount.find_opt r acc with
      | None -> failwith "row count is not properlty initialized"
      | Some v -> RowCount.add r (v + 1) acc)
    init l
  |> RowCount.for_all (fun _ v -> v >= Rules.min_players_on_row)

let count m = m |> PosMap.to_list |> List.length

let can_place m p r c =
  let m' = place p r c m in
  let remain = Rules.players_on_board - count m' in
  let min_def_reachable =
    List.length (player_on_rows m' (row Position.Defender)) + remain
  in
  let min_mid_reachable =
    List.length (player_on_rows m' (row Position.Midfielder)) + remain
  in
  let min_fwd_reachable =
    List.length (player_on_rows m' (row Position.Forward)) + remain
  in
  min_def_reachable >= Rules.min_players_on_row
  && min_mid_reachable >= Rules.min_players_on_row
  && min_fwd_reachable >= Rules.min_players_on_row

let hash board =
  let prime = 31 in
  Row.all
  |> List.fold_left
       (fun acc r ->
         Column.all
         |> List.fold_left
              (fun acc c ->
                let v =
                  match PosMap.find_opt (r, c) board with
                  | None -> 0
                  | Some _ -> 1
                in
                (acc * prime) + v)
              acc)
       17

let rotate board =
  to_list board
  |> List.map (fun (p, r, c) ->
      let new_c =
        Column.of_int_exn (Column.to_int Column.max - Column.to_int c)
      in
      (p, r, new_c))
  |> of_list

module ColSet = Set.Make (Column)

let rows_match ~left ~left_pos ~right ~right_pos =
  let filter_pos r p = Row.equal r (row p) in
  let occupied_cols pos b =
    b |> PosMap.to_list
    |> List.filter (fun ((r, _), _) -> filter_pos r pos)
    |> List.filter (fun ((_, _), cell) ->
        match cell with Empty -> false | Occupied _ -> true)
    |> List.map (fun ((_, c), _) -> c)
  in
  let left_cells = left |> occupied_cols left_pos |> ColSet.of_list in
  let right_cells =
    right |> occupied_cols right_pos
    |> List.map (fun c ->
        Column.of_int_exn (Column.to_int Column.max - Column.to_int c))
    |> ColSet.of_list
  in
  List.length (ColSet.inter left_cells right_cells |> ColSet.to_list)
  = List.length (ColSet.to_list right_cells)

let defenders_cover ~left ~right =
  rows_match ~left ~left_pos:Position.Defender ~right
    ~right_pos:Position.Forward
