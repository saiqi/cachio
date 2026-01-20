type t = { id : Ai_id.t; roster : Roster.t; strategy : Lineup_strategy.t }

let create id roster strategy = { id; roster; strategy }
let id a = a.id
let roster a = a.roster
let strategy a = a.strategy
let build_board a = Lineup_strategy.build (strategy a) (roster a)
