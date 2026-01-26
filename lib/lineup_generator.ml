let rec combinations k l =
  match l with
  | [] -> Seq.empty
  | x :: xs ->
      if k = 1 then Seq.return [ x ]
      else
        Seq.append
          (Seq.map (fun tail -> x :: tail) (combinations (k - 1) xs))
          (combinations k xs)

let all (type a) (module R : Rng.S with type t = a) (rng : a) roster =
  let players = Roster.to_list roster |> Utils.shuffle (module R) rng in
  let coords =
    Row.all
    |> List.concat_map (fun r -> List.map (fun c -> (r, c)) Column.all)
    |> Utils.shuffle (module R) rng
  in
  let configs =
    combinations Rules.players_on_board coords
    |> Seq.filter Board.is_shape_valid
  in
  combinations Rules.players_on_board players
  |> Seq.flat_map (fun ps ->
      let p_ids = List.map Player.id ps in
      configs
      |> Seq.map (fun placements ->
          List.fold_left2
            (fun board p_id (row, col) -> Board.place p_id row col board)
            Board.empty p_ids placements))
