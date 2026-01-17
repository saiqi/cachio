open Cachio

let test_round_robin () =
  let ids = List.init 6 Fun.id |> List.map Ai_id.of_int in
  let schedule = Schedule.round_robin ids in
  let days = List.init (Schedule.length schedule) Fun.id in
  Alcotest.check Alcotest.int "total length" 10 (Schedule.length schedule);
  List.iter
    (fun i ->
      let games = Schedule.day schedule i in
      Alcotest.check Alcotest.int
        (string_of_int i ^ "-day length")
        3 (List.length games);
      let ids =
        List.fold_left
          (fun acc g ->
            let h = Schedule.home g |> Ai_id.to_int in
            let a = Schedule.away g |> Ai_id.to_int in
            h :: a :: acc)
          [] games
      in

      Alcotest.check
        (Alcotest.list Alcotest.int)
        (string_of_int i ^ "-day check all participants")
        [ 0; 1; 2; 3; 4; 5 ]
        (List.sort Int.compare ids))
    days

let suite = [ ("round robin", `Quick, test_round_robin) ]
