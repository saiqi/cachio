open Cachio

let test () =
  let player =
    Player.create (Player_id.of_int 0) Position.Defender (Score.of_int_exn 3)
  in
  let roster = Roster.add player Roster.empty in
  match Roster.find_opt (Player.id player) roster with
  | Some p ->
      Alcotest.check Alcotest.int "player id matches"
        (Player.id p |> Player_id.to_int)
        0
  | None -> Alcotest.fail "player id does not match"

let suite = [ ("add player", `Quick, test) ]
