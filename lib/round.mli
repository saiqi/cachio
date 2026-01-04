val compute_position_score :
  board:Board.t -> roster:Roster.t -> position:Position.t -> int

val compute_param :
  home:bool -> board:Board.t -> roster:Roster.t -> Round_param.t

val resolve :
  roll:Dice.roll -> home:Round_param.t -> away:Round_param.t -> int * int
