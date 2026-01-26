type t = { id : Ai_id.t; roster : Roster.t; strategy : Strategy_id.t }

let create id roster strategy = { id; roster; strategy }
let id a = a.id
let roster a = a.roster
let strategy a = a.strategy

let build_board (type a) (module R : Rng.S with type t = a) (rng : a) a home =
  Lineup_strategy.build
    (module R)
    rng
    (Lineup_strategy.of_id ~home (strategy a))
    (roster a)
