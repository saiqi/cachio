type t = { id : Ai_id.t; param : Round_param.t; strategy : Strategy_id.t }

let create id strategy =
  { id; param = Scripted_strategy.of_id strategy; strategy }

let id s = s.id
let param s = s.param
let strategy s = s.strategy
