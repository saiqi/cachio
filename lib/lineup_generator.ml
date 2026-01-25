type roster_key = Player_id.t list

let roster_key_of_roster roster =
  Roster.to_list roster |> List.map Player.id |> List.sort Player_id.compare

module RosterKey = struct
  type t = roster_key

  let equal = ( = )
  let hash = Hashtbl.hash
end

module Cache = Hashtbl.Make (RosterKey)

let cache : Board.t list Cache.t = Cache.create 128

let rec combinations k = function
  | [] -> []
  | x :: xs ->
      if k = 1 then [ [ x ] ]
      else
        List.map (fun l -> x :: l) (combinations (k - 1) xs) @ combinations k xs

let all_coords =
  List.concat_map (fun r -> List.map (fun c -> (r, c)) Column.all) Row.all

let compute_all roster =
  let players = Roster.to_list roster in
  let player_sets = combinations Rules.players_on_board players in
  let coords = all_coords in
  let candidates =
    List.concat_map
      (fun ps ->
        let p_ids = List.map Player.id ps in
        let pos_sets = combinations Rules.players_on_board coords in

        List.map
          (fun placements ->
            List.fold_left2
              (fun board p_id (row, col) -> Board.place p_id row col board)
              Board.empty p_ids placements)
          pos_sets)
      player_sets
  in
  List.filter Board.is_valid candidates

let all roster =
  let key = roster_key_of_roster roster in
  match Cache.find_opt cache key with
  | Some boards -> boards
  | None ->
      let boards = compute_all roster in
      Cache.add cache key boards;
      boards
