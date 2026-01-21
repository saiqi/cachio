let run_with_audit (type a) (module R : Rng.S with type t = a) (rng : a)
    participants schedule =
  let ids = Participants.to_list participants |> List.map Ai.id in
  let standing, audits =
    Schedule.to_list schedule
    |> List.fold_left
         (fun acc day ->
           List.fold_left
             (fun (stand, audit) game ->
               let home = Participants.find (Schedule.home game) participants in
               let away = Participants.find (Schedule.away game) participants in
               let result = Game.play_with_audit (module R) rng ~home ~away in
               ( Standing.update stand (Game_audit.result result),
                 result :: audit ))
             acc day)
         (Standing.init ids, [])
  in
  (standing, List.rev audits)

let run (type a) (module R : Rng.S with type t = a) (rng : a) participants
    schedule =
  let standing, _ = run_with_audit (module R) rng participants schedule in
  standing
