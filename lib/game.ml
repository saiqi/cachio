let play (type a) (module R : Rng.S with type t = a) (rng : a) ~home ~away =
  let home_param =
    Round.compute_param ~home:true ~board:(Ai.build_board home)
      ~roster:(Ai.roster home)
  in
  let away_param =
    Round.compute_param ~home:false ~board:(Ai.build_board away)
      ~roster:(Ai.roster away)
  in
  let home_goals, away_goals =
    Round.resolve (module R) rng ~home:home_param ~away:away_param
  in
  Game_result.create ~home:(Ai.id home) ~home_goals ~away:(Ai.id away)
    ~away_goals
