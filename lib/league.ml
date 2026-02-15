let run_with_audit (type a) (module R : Rng.S with type t = a) (rng : a)
    participants schedule =
  let ids = Participants.to_list participants |> List.map Participant.id in
  let participants', standing, audits =
    Schedule.to_list schedule
    |> List.fold_left
         (fun acc day ->
           List.fold_left
             (fun (part, stand, audit) game ->
               let home = Participants.find (Schedule.home game) participants in
               let away = Participants.find (Schedule.away game) participants in
               let result = Game.play_with_audit (module R) rng ~home ~away in
               let home' =
                 Participant.after_game (Game_audit.result result) home
               in
               let away' =
                 Participant.after_game (Game_audit.result result) away
               in
               let part' =
                 part |> Participants.add home' |> Participants.add away'
               in
               let stand' = Standing.update stand (Game_audit.result result) in
               (part', stand', result :: audit))
             acc day)
         (participants, Standing.init ids, [])
  in
  (participants', standing, List.rev audits)

let run (type a) (module R : Rng.S with type t = a) (rng : a) participants
    schedule =
  let _, standing, _ = run_with_audit (module R) rng participants schedule in
  standing
