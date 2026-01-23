type t = { id : Ai_id.t; roster : Roster.t; strategy : Strategy_id.t }

let create id roster strategy = { id; roster; strategy }
let id a = a.id
let roster a = a.roster
let strategy a = a.strategy

let build_board a home =
  Lineup_strategy.build (Lineup_strategy.of_id ~home (strategy a)) (roster a)
