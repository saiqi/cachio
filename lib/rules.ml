let is_injured p = Shape.to_int (Player.shape p) = Shape.min

let adjust_score p pos =
  let score =
    match Player.pos p with
    | Position.Defender -> (
        match pos with
        | Position.Defender -> Player.score p
        | Position.Midfielder -> Score.decr (Player.score p)
        | Position.Forward -> Score.decr (Score.decr (Player.score p)))
    | Position.Midfielder -> (
        match pos with
        | Position.Defender -> Score.decr (Player.score p)
        | Position.Midfielder -> Player.score p
        | Position.Forward -> Score.decr (Player.score p))
    | Position.Forward -> (
        match pos with
        | Position.Defender -> Score.decr (Score.decr (Player.score p))
        | Position.Midfielder -> Score.decr (Player.score p)
        | Position.Forward -> Player.score p)
  in
  if is_injured p then Score.of_int_exn Score.min else score

let has_scored att def = att > def
let adjust_home_advantage h = if h then 1 else 0
let min_players_on_row = 1
let players_on_board = 6
let players_on_roster = 12
