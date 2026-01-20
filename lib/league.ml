let run (type a) (module R : Rng.S with type t = a) (rng : a) participants
    schedule =
  let ids = Participants.to_list participants |> List.map Ai.id in
  let standing = Standing.init ids in
  Schedule.to_list schedule
  |> List.fold_left
       (fun acc day ->
         List.fold_left
           (fun acc game ->
             let home = Participants.find (Schedule.home game) participants in
             let away = Participants.find (Schedule.away game) participants in
             let result = Game.play (module R) rng ~home ~away in
             Standing.update acc result)
           acc day)
       standing
