let rec combinations k = function
  | [] -> []
  | x :: xs ->
      if k = 1 then [ [ x ] ]
      else
        List.map (fun l -> x :: l) (combinations (k - 1) xs) @ combinations k xs

let all_coords =
  List.concat_map (fun r -> List.map (fun c -> (r, c)) Column.all) Row.all

let all_configs =
  all_coords
  |> combinations Rules.players_on_board
  |> List.filter Board.is_shape_valid

let all (type a) (module R : Rng.S with type t = a) (rng : a) roster =
  let players = Roster.to_list roster |> Utils.shuffle (module R) rng in
  let player_sets =
    combinations Rules.players_on_board players |> Utils.shuffle (module R) rng
  in
  let configs = all_configs |> Utils.shuffle (module R) rng in
  let candidates =
    List.concat_map
      (fun ps ->
        let p_ids = List.map Player.id ps in

        List.map
          (fun placements ->
            List.fold_left2
              (fun board p_id (row, col) -> Board.place p_id row col board)
              Board.empty p_ids placements)
          configs)
      player_sets
  in
  candidates
