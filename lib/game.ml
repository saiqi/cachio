let play_with_audit (type a) (module R : Rng.S with type t = a) (rng : a) ~home
    ~away =
  let home_param, home_board =
    Participant.reveal (module R) rng ~home:true home
  in
  let away_param, away_board =
    Participant.reveal (module R) rng ~home:false away
  in
  let home_param', away_param' =
    Round.adjust_param ~left:home_board ~left_param:home_param ~right:away_board
      ~right_param:away_param
  in
  let home_goals, away_goals =
    Round.resolve (module R) rng ~home:home_param' ~away:away_param'
  in
  let opt_players board = board |> Option.fold ~none:[] ~some:Board.players in
  let result =
    Game_result.create ~home:(Participant.id home) ~home_goals
      ~away:(Participant.id away) ~away_goals
      ~home_players:(opt_players home_board)
      ~away_players:(opt_players away_board)
  in
  Game_audit.create ~result ~home_param:home_param' ~away_param:away_param'
    ~home_strategy:(Participant.strategy home)
    ~away_strategy:(Participant.strategy away)
    ~home_board ~away_board

let play (type a) (module R : Rng.S with type t = a) (rng : a) ~home ~away =
  play_with_audit (module R) rng ~home ~away |> Game_audit.result
