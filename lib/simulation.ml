type t = { participants : Participants.t; schedule : Schedule.t }

let of_ais ais =
  let participants =
    List.fold_left
      (fun acc ai -> Participants.add ai acc)
      Participants.empty ais
  in
  let schedule = Schedule.round_robin (List.map Ai.id ais) in
  { participants; schedule }

let run (type a) (module R : Rng.S with type t = a) (rng : a) sim =
  League.run_with_audit (module R) rng sim.participants sim.schedule

let run_n n (type a) (module R : Rng.S with type t = a) (rng : a) sim =
  let rec aux k acc =
    if k <= 0 then List.rev acc
    else
      let res = run (module R) rng sim in
      aux (k - 1) (res :: acc)
  in
  aux n []
